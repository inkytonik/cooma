package org.bitbucket.inkytonik.cooma.primitive

import java.io.{BufferedInputStream, File, FileInputStream}
import java.sql.{Connection, DriverManager}
import java.util.Base64

import org.bitbucket.inkytonik.cooma.Backend
import org.bitbucket.inkytonik.cooma.CoomaException._
import org.bitbucket.inkytonik.cooma.primitive.Database.{Column, ConnectionData, DatabaseType}
import play.api.libs.json.Json

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
                                    case DatabaseType.Boolean =>
                                        prefix.contains("BOOL") || prefix.contains("INT")
                                    case DatabaseType.Int =>
                                        prefix.contains("INT")
                                    case DatabaseType.String =>
                                        true
                                    case x =>
                                        // exhaustivity checker is confused
                                        throw new MatchError(x)
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
                                Some(s"column '$header' has type '$aTypename', incompatible with '$eTypename'")
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

    def dbTableAll(index : Int, tablename : String) : ValueR =
        // check database exists
        connectionData.get(index) match {
            case Some(ConnectionData(connection, tables)) =>
                // check table exists
                tables.get(tablename) match {
                    case Some(columns) =>
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
                                                case DatabaseType.Boolean =>
                                                    varR(if (result.getBoolean(columnName)) "True" else "False", uniR)
                                                case DatabaseType.Int =>
                                                    intR(result.getInt(columnName))
                                                case DatabaseType.String =>
                                                    strR(result.getString(columnName))
                                                case x =>
                                                    // exhaustivity checker is confused
                                                    throw new MatchError(x)
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
                    case None =>
                        errPrim("DatabaseClient", s"table '$tablename' does not exist or cannot be accessed")
                }
            case None =>
                errPrim("DatabaseClient", s"argument $index is not a DatabaseClient")
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

    type DatabaseType = DatabaseType.Value

    object DatabaseType extends Enumeration {

        val Boolean = Value
        val Int = Value
        val String = Value

    }

    implicit lazy val fmtDatabaseType = Json.formatEnum(DatabaseType)
    implicit lazy val fmtColumn = Json.format[Column]

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
