package org.bitbucket.inkytonik.cooma.primitive.database

import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
import org.bitbucket.inkytonik.cooma.SymbolTable.{BoolT, IntT, StrT}
import play.api.libs.json._

sealed trait DbType extends Product {

    lazy val (atomic, nullable) : (DbType.Atomic, Boolean) =
        this match {
            case DbType.Nullable(tipe) => (tipe, true)
            case tipe : DbType.Atomic  => (tipe, false)
        }

    lazy val toCoomaString : String =
        this match {
            case DbType.Boolean        => "Boolean"
            case DbType.Integer        => "Int"
            case DbType.String         => "String"
            case DbType.Nullable(tipe) => s"Option(${tipe.toCoomaString})"
        }

}

object DbType {

    sealed trait Atomic extends DbType

    case object Boolean extends Atomic

    case object Integer extends Atomic

    case object String extends Atomic

    case class Nullable(tipe : Atomic) extends DbType

    def fromCooma(tipe : Expression) : Option[DbType] = {
        def getAtomicType(tipe : Expression) : Option[Atomic] =
            tipe match {
                case BoolT() => Some(Boolean)
                case IntT()  => Some(Integer)
                case StrT()  => Some(String)
                case _       => None
            }
        tipe match {
            case App(Idn(IdnUse("Option")), Vector(tipe)) => getAtomicType(tipe).map(Nullable)
            case tipe                                     => getAtomicType(tipe)
        }
    }

    implicit lazy val fmtDbType =
        new OFormat[DbType] {

            case class DbTypeSpec(
                t : java.lang.String,
                n : scala.Boolean
            )

            override def reads(json : JsValue) : JsResult[DbType] =
                Json.reads[DbTypeSpec]
                    .map {
                        case DbTypeSpec(t, n) =>
                            val atomic =
                                t match {
                                    case "Boolean" => Boolean
                                    case "Integer" => Integer
                                    case "String"  => String
                                }
                            if (n) DbType.Nullable(atomic) else atomic
                    }
                    .reads(json)

            override def writes(o : DbType) : JsObject =
                Json.writes[DbTypeSpec]
                    .contramap[DbType] {
                        case tipe : Atomic  => DbTypeSpec(tipe.toString, false)
                        case Nullable(tipe) => DbTypeSpec(tipe.toString, true)
                    }
                    .writes(o)

        }

}
