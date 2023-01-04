package org.bitbucket.inkytonik.cooma.primitive.database

import java.sql.{Connection, ResultSet}

import scala.annotation.tailrec

object Query {

  def getColumn(
      result: ResultSet,
      column: Metadata.Column
  ): (String, DbValue) = {
    val Metadata.Column(name, tipe) = column
    def getAtomicValue(tipe: DbType.Atomic): DbValue.Atomic =
      tipe match {
        case DbType.Boolean => DbValue.Boolean(result.getBoolean(name))
        case DbType.Integer => DbValue.Integer(result.getInt(name))
        case DbType.String  => DbValue.String(result.getString(name))
      }
    val value =
      tipe match {
        case DbType.Nullable(tipe) =>
          val atomicValue = getAtomicValue(tipe)
          if (result.wasNull()) DbValue.Null
          else DbValue.NotNull(atomicValue)
        case tipe: DbType.Atomic =>
          getAtomicValue(tipe)
      }
    name -> value
  }

  def all(
      conn: Connection,
      table: Metadata.Table
  ): Seq[Seq[(String, DbValue)]] = {
    val Metadata.Table(tablename, columns) = table
    val query = s"SELECT * FROM $tablename;"
    val result = conn.prepareStatement(query).executeQuery()
    @tailrec
    def aux(out: Seq[Seq[(String, DbValue)]]): Seq[Seq[(String, DbValue)]] =
      if (result.next()) aux(out :+ columns.map(getColumn(result, _)))
      else out
    aux(Seq.empty)
  }

  def getById(
      conn: Connection,
      table: Metadata.Table,
      id: DbValue.Integer
  ): Option[Seq[(String, DbValue)]] = {
    val Metadata.Table(tablename, columns) = table
    val query = s"SELECT * FROM $tablename WHERE id = ${id.toSql};"
    val result = conn.prepareStatement(query).executeQuery()
    if (result.next()) Some(columns.map(getColumn(result, _)))
    else None
  }

  def insert(
      conn: Connection,
      table: Metadata.Table,
      row: Seq[(String, DbValue)]
  ): Int = {
    val Metadata.Table(tablename, _) = table
    val (headers, values) = row.unzip
    val headersString = headers.mkString("(", ", ", ")")
    val valuesString = values.map(_.toSql).mkString("(", ", ", ")")
    val query = s"INSERT INTO $tablename $headersString VALUES $valuesString;"
    conn.prepareStatement(query).executeUpdate()
  }

  def update(
      conn: Connection,
      table: Metadata.Table,
      row: Seq[(String, DbValue)]
  ): Int = {
    val Metadata.Table(tablename, _) = table
    val (keys, nonKeys) = row.partition { case (header, _) => header == "id" }
    val (_, id) = keys.head
    val query =
      nonKeys.iterator
        .map { case (header, value) => s"$header = ${value.toSql}" }
        .mkString(
          s"UPDATE $tablename SET ",
          ", ",
          s" WHERE id = ${id.toSql};"
        )
    conn.prepareStatement(query).executeUpdate()
  }

  def delete(
      conn: Connection,
      table: Metadata.Table,
      id: DbValue.Integer
  ): Int = {
    val Metadata.Table(tablename, _) = table
    val query = s"DELETE FROM $tablename WHERE id = ${id.toSql};"
    conn.prepareStatement(query).executeUpdate()
  }

}
