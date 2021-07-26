package org.bitbucket.inkytonik.cooma.primitive

import java.sql.Connection

import org.bitbucket.inkytonik.cooma.Backend
import org.bitbucket.inkytonik.cooma.primitive.Database.ConnectionData
import org.bitbucket.inkytonik.cooma.CoomaException._

import scala.annotation.tailrec

trait Database {

    self : Backend =>

    var data : Option[ConnectionData] = None

    def dbTableAll(rho : Env) : ValueR =
        data match {
            case Some(ConnectionData(connection, tablename, headers)) =>
                val sql = f"SELECT * FROM `$tablename`;"
                val result = connection.prepareStatement(sql).executeQuery()
                @tailrec
                def aux(out : Vector[ValueR]) : ValueR =
                    if (result.next()) {
                        val row =
                            headers.map { header =>
                                val value = Option(result.getString(header)).getOrElse("")
                                fldR(header, strR(value))
                            }.toVector
                        aux(out :+ recR(row))
                    } else vecR(out)
                aux(Vector.empty)
            case None =>
                errPrim("DatabaseClient", "no connection")
        }

    override def finalize() : Unit = {
        super.finalize()
        data.foreach(_.connection.close())
    }

}

object Database {

    case class ConnectionData(
        connection : Connection,
        tablename : String,
        headers : Seq[String]
    ) {

        require(
            tablename.forall(c => c.isLetterOrDigit || c == '_'),
            s"invalid tablename $tablename"
        )

    }

}
