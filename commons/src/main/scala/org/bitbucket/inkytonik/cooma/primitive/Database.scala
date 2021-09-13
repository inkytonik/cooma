package org.bitbucket.inkytonik.cooma.primitive

import java.io.{BufferedInputStream, File, FileInputStream}
import java.sql.{Connection, DriverManager}

import org.bitbucket.inkytonik.cooma.Backend
import org.bitbucket.inkytonik.cooma.CoomaException._
import org.bitbucket.inkytonik.cooma.primitive.Database.ConnectionData

import scala.annotation.tailrec
import scala.collection.mutable

trait Database {

    self : Backend =>

    val SqliteHeader = "SQLite format 3".getBytes :+ 0.toByte

    val connectionData : mutable.Map[Int, ConnectionData] = mutable.Map.empty

    def dbConfigure(path : String, tables : Map[String, Vector[String]], index : Int) : Unit = {
        Class.forName("org.sqlite.JDBC")
        // validate path and get connection
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
        for ((tablename, columns) <- tables) {
            // check table name valid
            if (tablename.exists(c => !c.isLetterOrDigit && c != '_'))
                errCap("DatabaseClient", s"tablename '$tablename' contains invalid characters")
            // list column names
            val query = s"PRAGMA table_info($tablename)"
            val result = connection.prepareStatement(query).executeQuery()
            @tailrec
            def getHeaders(out : Set[String]) : Set[String] =
                if (result.next()) getHeaders(out + result.getString("name"))
                else out
            val headers = getHeaders(Set.empty)
            // check table exists
            if (headers.isEmpty)
                errCap("DatabaseClient", s"table $tablename does not exist")
            // check table has all desired columns
            val invalid = columns.toSet -- headers
            if (invalid.nonEmpty)
                errCap("DatabaseClient", invalid.mkString(s"table $tablename is missing columns: ", ", ", ""))
        }
        connectionData += (index -> ConnectionData(connection, tables))
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
                                val row = columns.map { columnName =>
                                    val value = Option(result.getString(columnName)).getOrElse("")
                                    fldR(columnName, strR(value))
                                }
                                aux(out :+ recR(row))
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
        tables : Map[String, Vector[String]]
    )

    def encodeSpec(index : Int, tables : Seq[(String, Vector[String])]) : String =
        tables
            .map { case (tablename, columns) => columns.mkString(s"$tablename:", ",", "") }
            .mkString(s"DatabaseClient::$index::", ":", "")

    def decodeSpec(spec : String) : Option[(Int, Seq[(String, Vector[String])])] =
        spec.split("::").toSeq match {
            case "DatabaseClient" +: index +: tables =>
                @tailrec
                def aux(
                    tables : Seq[String],
                    out : Seq[(String, Vector[String])]
                ) : Option[Seq[(String, Vector[String])]] =
                    tables match {
                        case hd +: tl =>
                            hd.split(':').toSeq match {
                                case tablename +: columns +: Nil =>
                                    aux(tl, out :+ (tablename -> columns.split(',').toVector))
                                case _ =>
                                    None
                            }
                        case _ =>
                            Some(out)
                    }
                for {
                    tables <- aux(tables, Seq.empty)
                    index <- index.toIntOption
                } yield (index, tables)
            case _ => None
        }

}
