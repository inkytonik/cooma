package org.bitbucket.inkytonik.cooma.primitive.database

import java.util.Base64

import org.bitbucket.inkytonik.cooma.primitive.database.Metadata.Table
import play.api.libs.json.{Format, Json}

case class Metadata(tables : Seq[Table]) {

    def toSpec : String =
        Base64.getEncoder.encodeToString(Json.toJson(this).toString.getBytes)

}

object Metadata {

    def fromSpec(spec : String) : Metadata =
        Json.parse(Base64.getDecoder.decode(spec)).as[Metadata]

    case class Table(
        name : String,
        columns : Seq[Column]
    )

    case class Column(
        name : String,
        tipe : DbType
    )

    implicit lazy val fmtColumn : Format[Column] = Json.format
    implicit lazy val fmtTable : Format[Table] = Json.format
    implicit lazy val fmtMetadata : Format[Metadata] = Json.format

}
