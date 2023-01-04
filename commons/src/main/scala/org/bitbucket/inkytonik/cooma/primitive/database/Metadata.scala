package org.bitbucket.inkytonik.cooma.primitive.database

import java.util.Base64

import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
import org.bitbucket.inkytonik.cooma.primitive.database.Metadata.Table
import play.api.libs.json.{Format, Json}

import scala.annotation.tailrec

case class Metadata(tables: Seq[Table]) {

  def toSpec: String =
    Base64.getEncoder.encodeToString(Json.toJson(this).toString.getBytes)

}

object Metadata {

  def fromCooma(tables: Seq[FieldType]): Option[Metadata] = {
    @tailrec
    def auxTables(
        coomaTables: Seq[FieldType],
        out: Seq[Table] = Seq.empty
    ): Option[Seq[Table]] = {
      @tailrec
      def auxColumns(
          coomaColumns: Seq[FieldType],
          out: Seq[Column] = Seq.empty
      ): Option[Seq[Column]] =
        coomaColumns match {
          case FieldType(name, tipe) +: tl =>
            DbType.fromCooma(tipe) match {
              case Some(dbType) => auxColumns(tl, out :+ Column(name, dbType))
              case None         => None
            }
          case _ =>
            Some(out)
        }
      coomaTables match {
        case FieldType(
              name,
              App(Idn(IdnUse("Table")), Vector(RecT(coomaColumns)))
            ) +: tl =>
          auxColumns(coomaColumns) match {
            case Some(columns) => auxTables(tl, out :+ Table(name, columns))
            case None          => None
          }
        case _ =>
          Some(out)
      }
    }
    auxTables(tables).map(Metadata(_))
  }

  def fromSpec(spec: String): Metadata =
    Json.parse(Base64.getDecoder.decode(spec)).as[Metadata]

  case class Table(
      name: String,
      columns: Seq[Column]
  )

  case class Column(
      name: String,
      tipe: DbType
  )

  implicit lazy val fmtColumn: Format[Column] = Json.format
  implicit lazy val fmtTable: Format[Table] = Json.format
  implicit lazy val fmtMetadata: Format[Metadata] = Json.format

}
