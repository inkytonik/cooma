package org.bitbucket.inkytonik.cooma.primitive.database

import java.io.{BufferedInputStream, File, FileInputStream}
import java.sql.{Connection, DriverManager}

import org.bitbucket.inkytonik.cooma.primitive.database.Metadata.{Column, Table}

import scala.annotation.tailrec

object Validation {

    case class SqlColumn(
        name : String,
        tipe : String,
        nullable : Boolean,
        primaryKey : Boolean
    )

    def validateColumn(expected : Column, actual : SqlColumn) : Seq[String] = {
        val Column(name, tipe) = expected
        val SqlColumn(_, aType, aNullable, aPrimaryKey) = actual
        val typeError = {
            val prefix = aType.toUpperCase.filter(_.isLetter)
            val compatible =
                tipe.atomic match {
                    case DbType.Boolean =>
                        prefix.contains("BOOL") || prefix.contains("INT")
                    case DbType.Integer =>
                        prefix.contains("INT")
                    case DbType.String =>
                        true
                }
            if (compatible) Seq.empty
            else Seq(s"column '$name' has type '$aType', incompatible with ${tipe.atomic.toCoomaString}")
        }
        val nullableError =
            (tipe.nullable, aNullable) match {
                case (true, false) => Seq(s"column '$name' is non-nullable")
                case (false, true) => Seq(s"column '$name' is nullable")
                case _             => Seq.empty
            }
        val primaryKeyError =
            (name, aPrimaryKey) match {
                case ("id", true)  => Seq.empty
                case ("id", false) => Seq(s"column 'id' is not a primary key")
                case (name, true)  => Seq(s"column '$name' is a primary key")
                case (name, false) => Seq.empty
            }
        typeError ++ nullableError ++ primaryKeyError
    }

    def validateTable(table : Table, connection : Connection) : Seq[String] = {
        val Table(name, expectedColumns) = table
        val errors =
            for {
                _ <- {
                    // check table name valid
                    val valid = name.forall(c => c.isLetterOrDigit || c == '_')
                    if (valid) Right(())
                    else Left(Seq(s"'$name' contains invalid characters"))
                }
                _ <- {
                    // check primary key
                    expectedColumns.find(_.name == "id") match {
                        case Some(Column(_, DbType.Integer)) => Right(())
                        case Some(_)                         => Left(Seq("column 'id' must have type 'Int'"))
                        case None                            => Left(Seq("table must have 'id' column"))
                    }
                }
                actualColumns = {
                    val query = s"PRAGMA table_info($name)"
                    val result = connection.prepareStatement(query).executeQuery()
                    @tailrec
                    def getColumns(out : Map[String, SqlColumn]) : Map[String, SqlColumn] =
                        if (result.next()) {
                            val name = result.getString("name")
                            val tipe = result.getString("tipe")
                            val nullable = !result.getBoolean("notnull")
                            val primaryKey = result.getBoolean("pk")
                            val actualColumn = SqlColumn(name, tipe, nullable, primaryKey)
                            getColumns(out + (name -> actualColumn))
                        } else {
                            result.close()
                            out
                        }
                    getColumns(Map.empty)
                }
                _ <- {
                    // check table exists
                    if (actualColumns.isEmpty) Left(Seq("the table does not exist"))
                    else Right(())
                }
                _ <- {
                    // validate columns
                    val errors =
                        expectedColumns.flatMap {
                            case expected @ Column(name, _) =>
                                actualColumns.get(name) match {
                                    case Some(actual) => validateColumn(expected, actual)
                                    case None         => Seq(s"column '$name' does not exist")
                                }
                        }
                    if (errors.isEmpty) Right(())
                    else Left(errors)
                }
            } yield ()
        errors.left.getOrElse(Seq.empty)
    }

    def validateTables(tables : Seq[Table], connection : Connection) : Option[String] = {
        val tablesWithErrors =
            tables.flatMap {
                case table @ Table(name, _) =>
                    val errors = validateTable(table, connection)
                    if (errors.isEmpty) None
                    else Some(name -> errors)
            }
        if (tablesWithErrors.nonEmpty) {
            val t = " " * 4
            val message =
                tablesWithErrors
                    .flatMap { case (name, errors) => s"$t$name:" +: errors.map(e => s"$t$t$e") }
                    .mkString("the following tables have errors:\n", "\n", "")
            Some(message)
        } else None
    }

    def validateDatabase(path : String, metadata : Metadata, index : Int) : Option[String] = {
        val error =
            for {
                _ <- {
                    // check file exists
                    val file = new File(path)
                    if (file.isFile) Right(())
                    else Left(s"'$path' is not a file")
                }
                _ <- {
                    // check SQLite file
                    val bis = new BufferedInputStream(new FileInputStream(path))
                    val array = new Array[Byte](16)
                    bis.read(array)
                    bis.close()
                    val actual = array.toSeq
                    val expected = "SQLite format 3".getBytes.toSeq :+ 0.toByte
                    if (actual == expected) Right(())
                    else Left(s"'$path' is not an SQLite database")
                }
                connection = DriverManager.getConnection(s"jdbc:sqlite:$path")
                _ <- validateTables(metadata.tables, connection).toLeft(())
            } yield ()
        error.left.toOption
    }

}
