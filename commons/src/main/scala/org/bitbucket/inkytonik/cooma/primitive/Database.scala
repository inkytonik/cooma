package org.bitbucket.inkytonik.cooma.primitive

import java.sql.{Connection, SQLException}

import org.bitbucket.inkytonik.cooma.Backend
import org.bitbucket.inkytonik.cooma.CoomaException._
import org.bitbucket.inkytonik.cooma.primitive.database.{DbValue, Metadata, Query}
import org.bitbucket.inkytonik.cooma.primitive.database.Validation.validateDatabase

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

trait Database {

    self : Backend =>

    val connections : mutable.Map[Int, (Connection, Metadata)] = mutable.Map.empty

    private def err(msg : String) : Nothing =
        errPrim("DatabaseClient", msg)

    def dbConfigure(path : String, metadata : Metadata, index : Int) : Unit =
        validateDatabase(path, metadata, index) match {
            case Right(connection) => connections += (index -> ((connection, metadata)))
            case Left(error)       => err(error)
        }

    def dbAll(index : Int, tablename : String) : ValueR = {
        val (conn, table) = validateTable(index, tablename)
        val rows = Query.all(conn, table).map(_.toVector).toVector
        vecR(rows.map(row => recR(row.map { case (k, v) => fldR(k, v.toCooma(this)) })))
    }

    def dbGetById(index : Int, tablename : String, id : ValueR) : ValueR = {
        val (conn, table) = validateTable(index, tablename)
        val dbid = validateId(id)
        val row = Query.getById(conn, table, dbid).map(_.toVector)
        row match {
            case Some(row) => varR("Some", recR(row.map { case (k, v) => fldR(k, v.toCooma(this)) }))
            case None      => varR("None", uniR)
        }
    }

    def dbInsert(index : Int, tablename : String, row : ValueR) : ValueR = {
        val (conn, table) = validateTable(index, tablename)
        val dbrow = validateRow(table, row)
        resultToCooma(Query.insert(conn, table, dbrow))
    }

    def dbUpdate(index : Int, tablename : String, row : ValueR) : ValueR = {
        val (conn, table) = validateTable(index, tablename)
        val dbrow = validateRow(table, row)
        resultToCooma(Query.update(conn, table, dbrow))
    }

    def dbDelete(index : Int, tablename : String, id : ValueR) : ValueR = {
        val (conn, table) = validateTable(index, tablename)
        val dbid = validateId(id)
        resultToCooma(Query.delete(conn, table, dbid))
    }

    private def resultToCooma(result : => Int) : ValueR =
        Try(result) match {
            case Success(count) =>
                varR("Right", intR(count))
            case Failure(e : SQLException) =>
                varR("Left", recR(Vector(
                    fldR("code", intR(e.getErrorCode)),
                    fldR("message", strR(e.getMessage))
                )))
            case Failure(e) =>
                throw e
        }

    def validateTable(index : Int, tablename : String) : (Connection, Metadata.Table) =
        connections.get(index) match {
            case Some((connection, Metadata(tables))) =>
                tables.find(_.name == tablename) match {
                    case Some(table) => (connection, table)
                    case None        => err(s"table '$tablename' does not exist or cannot be accessed")
                }
            case None =>
                err(s"argument $index is not a DatabaseClient")
        }

    def validateId(id : ValueR) : DbValue.Integer =
        DbValue.fromCooma(this)(id) match {
            case Some(dbid : DbValue.Integer) => dbid
            case _                            => err(s"invalid database ID '$id'")
        }

    def validateRow(table : Metadata.Table, row : ValueR) : Seq[(String, DbValue)] =
        DbValue.rowFromCooma(this)(row) match {
            case Some(actualColumns) =>
                val expectedColumnsSorted = table.columns.sortBy(_.name)
                val actualColumnsSorted = actualColumns.sortBy { case (name, _) => name }
                val zipped = actualColumnsSorted.zip(expectedColumnsSorted)
                @tailrec
                def aux(columns : Seq[((String, DbValue), Metadata.Column)]) : Unit =
                    columns match {
                        case ((aName, aValue), Metadata.Column(eName, eType)) +: tl =>
                            if (aName != eName || !aValue.checkType(eType))
                                err("row does not conform to expected type")
                            else aux(tl)
                        case _ =>
                            ()
                    }
                aux(zipped)
                actualColumns
            case None =>
                err("invalid row")
        }

    override def finalize() : Unit = {
        super.finalize()
        connections.valuesIterator.foreach { case (connection, _) => connection.close() }
    }

}
