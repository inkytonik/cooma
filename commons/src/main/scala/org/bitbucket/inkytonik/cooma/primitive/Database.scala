package org.bitbucket.inkytonik.cooma.primitive

import java.io.{BufferedInputStream, File, FileInputStream}
import java.sql.{Connection, DriverManager}

import org.bitbucket.inkytonik.cooma.Backend
import org.bitbucket.inkytonik.cooma.CoomaException._
import org.bitbucket.inkytonik.cooma.primitive.Database.ConnectionData

import scala.annotation.tailrec

trait Database {

    self : Backend =>

    private val ArgumentRegex = """([^:]+):([a-zA-Z0-9_]+)""".r

    var connectionData : Option[ConnectionData] = None

    def dbConfigure(argument : String, desiredHeaders : Vector[String]) : Unit = {
        if (connectionData.isDefined)
            errCap("Table", "only one table per program is supported")
        Class.forName("org.sqlite.JDBC")
        val (path, tablename) = argument match {
            case ArgumentRegex(path, tablename) => (path, tablename)
            case x                              => errCap("Table", s"invalid table path '$x'")
        }
        val connection = {
            val file = new File(path)
            if (!file.isFile)
                errCap("Table", s"file $path does not exist")
            // check that this is an SQLite file
            val header = {
                val bis = new BufferedInputStream(new FileInputStream(file))
                val array = new Array[Byte](16)
                bis.read(array)
                bis.close()
                array.iterator
            }
            if (!header.zip("SQLite format 3".getBytes :+ 0).forall { case (e, a) => e == a })
                errCap("Table", s"$path is not an SQLite database")
            DriverManager.getConnection(s"jdbc:sqlite:$path")
        }
        // validate headers
        val query = s"PRAGMA table_info($tablename)"
        val result = connection.prepareStatement(query).executeQuery()
        @tailrec
        def getHeaders(out : Set[String]) : Set[String] =
            if (result.next()) getHeaders(out + result.getString("name"))
            else out
        val headers = getHeaders(Set.empty)
        if (headers.isEmpty)
            errCap("Table", s"table $tablename does not exist")
        val invalid = desiredHeaders.toSet -- headers
        if (invalid.nonEmpty)
            errCap("Table", invalid.mkString(s"table $tablename is missing columns: ", ", ", ""))
        connectionData = Some(ConnectionData(connection, tablename, desiredHeaders))
    }

    def dbTableAll(rho : Env) : ValueR =
        connectionData match {
            case Some(ConnectionData(connection, tablename, headers)) =>
                val query = f"SELECT * FROM `$tablename`;"
                val result = connection.prepareStatement(query).executeQuery()
                @tailrec
                def aux(out : Vector[ValueR]) : ValueR =
                    if (result.next()) {
                        val row = headers.map { header =>
                            val value = Option(result.getString(header)).getOrElse("")
                            fldR(header, strR(value))
                        }
                        aux(out :+ recR(row))
                    } else vecR(out)
                aux(Vector.empty)
            case None =>
                errPrim("DatabaseClient", "no connection")
        }

    override def finalize() : Unit = {
        super.finalize()
        connectionData.foreach(_.connection.close())
    }

}

object Database {

    case class ConnectionData(
        connection : Connection,
        tablename : String,
        headers : Vector[String]
    )

}
