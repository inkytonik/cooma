package org.bitbucket.inkytonik.cooma.primitive

import java.io.{BufferedInputStream, File, FileInputStream}
import java.sql.{Connection, DriverManager}
import java.util.Base64

import org.bitbucket.inkytonik.cooma.Backend
import org.bitbucket.inkytonik.cooma.CoomaException._
import org.bitbucket.inkytonik.cooma.primitive.Database.{Column, ConnectionData, DtBoolean, DtInt, DtString}
import play.api.libs.json._

import scala.annotation.tailrec
import scala.collection.mutable

trait Database {

    self : Backend =>

    val SqliteHeader = "SQLite format 3".getBytes :+ 0.toByte

    val connectionData : mutable.Map[Int, ConnectionData] = mutable.Map.empty

    def dbConfigure(path : String, tables : Map[String, Map[String, Column]], index : Int) : Unit = {
        Class.forName("org.sqlite.JDBC")
        val connection = {
            // check file exists
            val file = new File(path)
            if (!file.isFile)
                errCap("DatabaseClient", s"file $path does not exist")
            // check SQLite file
            val header = {
                val bis = new BufferedInputStream(new FileInputStream(file))
                val array = new Array[Byte](16)
                bis.read(array)
                bis.close()
                array.iterator
            }
            if (!header.zip(SqliteHeader).forall { case (a, b) => a == b })
                errCap("DatabaseClient", s"$path is not an SQLite database")
            // connect
            DriverManager.getConnection(s"jdbc:sqlite:$path")
        }
        // validate desired tables and respective columns
        for ((tablename, expectedColumns) <- tables) {
            // check table name valid
            if (tablename.exists(c => !c.isLetterOrDigit && c != '_'))
                errCap("DatabaseClient", s"tablename '$tablename' contains invalid characters")
            // list column names and types
            val query = s"PRAGMA table_info($tablename)"
            val result = connection.prepareStatement(query).executeQuery()
            @tailrec
            def getHeaders(out : Map[String, (String, Boolean)]) : Map[String, (String, Boolean)] =
                if (result.next()) {
                    val name = result.getString("name")
                    val typename = result.getString("type")
                    val nullable = !result.getBoolean("notnull")
                    val entry = name -> ((typename, nullable))
                    getHeaders(out + entry)
                } else out
            val actualColumns = getHeaders(Map.empty)
            // check table exists
            if (actualColumns.isEmpty)
                errCap("DatabaseClient", s"table $tablename does not exist")
            // check table has all desired columns and appropriate types
            val errors =
                (for ((header, Column(eTypename, eNullable)) <- expectedColumns.toSeq) yield {
                    // check existence
                    actualColumns.get(header) match {
                        case Some((aTypename, aNullable)) =>
                            // check type
                            val prefix = aTypename.toUpperCase.filter(_.isLetter)
                            val compatible =
                                eTypename match {
                                    case DtBoolean =>
                                        prefix.contains("BOOL") || prefix.contains("INT")
                                    case DtInt =>
                                        prefix.contains("INT")
                                    case DtString =>
                                        true
                                }
                            if (compatible) {
                                // check nullability
                                (eNullable, aNullable) match {
                                    case (true, false) =>
                                        Some(s"column '$header' is non-nullable")
                                    case (false, true) =>
                                        Some(s"column '$header' is nullable")
                                    case _ =>
                                        None
                                }
                            } else
                                Some(s"column '$header' has type '$aTypename', incompatible with '${eTypename.toString.drop(2)}'")
                        case None =>
                            Some(s"column '$header' does not exist")
                    }
                }).flatten
            if (errors.nonEmpty) {
                val message =
                    errors.mkString(
                        s"specification of table '$tablename' does not match actual table:\n    ",
                        "\n    ",
                        ""
                    )
                errCap("DatabaseClient", message)
            }
            connectionData += (index -> ConnectionData(connection, tables))
        }
    }

    def dbAccessTable[A](index : Int, tablename : String)(f : (Connection, Map[String, Column]) => A) : A =
        // check database exists
        connectionData.get(index) match {
            case Some(ConnectionData(connection, tables)) =>
                // check table exists
                tables.get(tablename) match {
                    case Some(columns) =>
                        f(connection, columns)
                    case None =>
                        errPrim("DatabaseClient", s"table '$tablename' does not exist or cannot be accessed")
                }
            case None =>
                errPrim("DatabaseClient", s"argument $index is not a DatabaseClient")
        }

    def dbTableAll(index : Int, tablename : String) : ValueR =
        dbAccessTable(index, tablename) { (connection, columns) =>
            // run query
            val query = f"SELECT * FROM `$tablename`;"
            val result = connection.prepareStatement(query).executeQuery()
            // get result
            @tailrec
            def aux(out : Vector[ValueR]) : ValueR =
                if (result.next()) {
                    val row = columns.iterator.map {
                        case (columnName, Column(t, n)) =>
                            val valueNonNullable =
                                t match {
                                    case DtBoolean =>
                                        varR(if (result.getBoolean(columnName)) "True" else "False", uniR)
                                    case DtInt =>
                                        intR(result.getInt(columnName))
                                    case DtString =>
                                        strR(result.getString(columnName))
                                }
                            val value =
                                (n, result.wasNull()) match {
                                    case (true, true)  => varR("None", uniR)
                                    case (true, false) => varR("Some", valueNonNullable)
                                    case (false, _)    => valueNonNullable
                                }
                            fldR(columnName, value)
                    }
                    aux(out :+ recR(row.toVector))
                } else vecR(out)
            aux(Vector.empty)
        }

    protected def toQuery(value : ValueR, column : Column) : String = {
        val Column(t, n) = column
        def getBaseQueryValue(value : ValueR) : String =
            t match {
                case DtBoolean =>
                    isVarR(value) match {
                        case Some(("True", _))  => "TRUE"
                        case Some(("False", _)) => "FALSE"
                        case _                  => errCap("DatabaseClient", "expected Boolean")
                    }
                case DtInt =>
                    isIntR(value).getOrElse(errCap("DatabaseClient", "expected Int")).toString
                case DtString =>
                    isStrR(value).getOrElse(errCap("DatabaseClient", "expected String"))
            }
        if (n)
            isVarR(value) match {
                case Some(("Some", value)) => getBaseQueryValue(value)
                case Some(("None", _))     => "NULL"
                case _                     => errCap("DatabaseClient", "expected Option")
            }
        else
            getBaseQueryValue(value)
    }

    def dbInsert(index : Int, tablename : String, value : ValueR) : ValueR =
        dbAccessTable(index, tablename) { (connection, columns) =>
            val record = isRecR(value).getOrElse(errCap("DatabaseClient", "non-record value"))
            val fields = record.map(isFldR(_).getOrElse(errCap("DatabaseClient", "non-field value")))
            val (headers, _) = fields.unzip
            val queryHeaders = headers.mkString("(", ", ", ")")
            val queryValues =
                fields
                    .map {
                        case (header, value) =>
                            val column = columns.getOrElse(header, errCap("DatabaseClient", s"missing column '$header'"))
                            toQuery(value, column)
                    }
                    .mkString("(", ", ", ")")
            val query = f"INSERT INTO $tablename $queryHeaders VALUES $queryValues;"
            connection.prepareStatement(query).executeUpdate()
            uniR // TODO
        }

    override def finalize() : Unit = {
        super.finalize()
        connectionData.valuesIterator.foreach(_.connection.close())
    }

}

object Database {

    case class ConnectionData(
        connection : Connection,
        tables : Map[String, Map[String, Column]]
    )

    case class Column(
        t : DatabaseType,
        n : Boolean
    )

    sealed trait DatabaseType extends Product

    case object DtBoolean extends DatabaseType
    case object DtInt extends DatabaseType
    case object DtString extends DatabaseType

    implicit lazy val fmtDatabaseType : Format[DatabaseType] = {
        val reads =
            Reads[DatabaseType] {
                case JsString("Boolean") => JsSuccess(DtBoolean)
                case JsString("Int")     => JsSuccess(DtInt)
                case JsString("String")  => JsSuccess(DtString)
                case _                   => JsError("Expected 'Boolean', 'Int', or 'String'")
            }
        val writes =
            Writes[DatabaseType] {
                case DtBoolean => JsString("Boolean")
                case DtInt     => JsString("Int")
                case DtString  => JsString("String")
            }
        Format(reads, writes)
    }

    implicit lazy val fmtColumn =
        Json.format[Column]

    def encodeSpec(index : Int, tables : Map[String, Map[String, Column]]) : String = {
        val tablesEncoded = Base64.getEncoder.encodeToString(Json.toJson(tables).toString.getBytes)
        s"DatabaseClient:$index:$tablesEncoded"
    }

    def decodeSpec(spec : String) : Option[(Int, Map[String, Map[String, Column]])] = {
        spec.split(':').toSeq match {
            case "DatabaseClient" +: index +: tablesEncoded +: Nil =>
                val json = Json.parse(Base64.getDecoder.decode(tablesEncoded))
                Some((index.toInt, json.as[Map[String, Map[String, Column]]]))
            case _ =>
                None
        }
    }

}
