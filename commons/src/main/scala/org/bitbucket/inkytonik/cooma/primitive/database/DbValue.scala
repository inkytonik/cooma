package org.bitbucket.inkytonik.cooma.primitive.database

import org.bitbucket.inkytonik.cooma.Backend

import scala.annotation.tailrec

sealed trait DbValue extends Product {

    def checkType(tipe : DbType) : Boolean =
        (this, tipe) match {
            case (_ : DbValue.Boolean, DbType.Boolean) |
                (_ : DbValue.Integer, DbType.Integer) |
                (_ : DbValue.String, DbType.String) |
                (DbValue.Null, DbType.Nullable(_)) => true
            case (DbValue.NotNull(value), DbType.Nullable(tipe)) => value.checkType(tipe)
            case _ => false
        }

    def toCooma(backend : Backend) : backend.ValueR = {
        import backend._
        this match {
            case DbValue.Boolean(boolean) => varR(if (boolean) "True" else "False", uniR)
            case DbValue.Integer(int)     => intR(int)
            case DbValue.String(string)   => strR(string)
            case DbValue.NotNull(value)   => varR("Some", value.toCooma(backend))
            case DbValue.Null             => varR("None", uniR)
        }
    }

    def toSql =
        this match {
            case DbValue.Boolean(boolean) => boolean.toString.toUpperCase
            case DbValue.Integer(int)     => int.toString
            case DbValue.String(string)   => s"'$string'"
            case DbValue.NotNull(value)   => value.toString
            case DbValue.Null             => "NULL"
        }

}

object DbValue {

    sealed trait Atomic extends DbValue

    case class Boolean(boolean : scala.Boolean) extends Atomic

    case class Integer(int : BigInt) extends Atomic

    case class String(string : java.lang.String) extends Atomic

    sealed trait Nullable[+A <: Atomic] extends DbValue

    case class NotNull[A <: Atomic](value : A) extends Nullable[A]

    case object Null extends Nullable[Nothing]

    def fromCooma(backend : Backend)(value : backend.ValueR) : Option[DbValue] = {
        import backend._
        def tryTrue(v : ValueR) =
            isVarR(v).flatMap {
                case ("True", unit) =>
                    isRecR(unit) match {
                        case Some(Vector()) => Some(Boolean(true))
                        case _              => None
                    }
                case _ =>
                    None
            }
        def tryFalse(v : ValueR) =
            isVarR(v).flatMap {
                case ("False", unit) =>
                    isRecR(unit) match {
                        case Some(Vector()) => Some(Boolean(false))
                        case _              => None
                    }
                case _ =>
                    None
            }
        def tryInteger(v : ValueR) =
            isIntR(v).map(Integer)
        def tryString(v : ValueR) =
            isStrR(v).map(String)
        def tryNotNull(v : ValueR) =
            isVarR(v) match {
                case Some(("Some", value)) => fromCooma(backend)(value)
                case _                     => None
            }
        def tryNull(v : ValueR) =
            isVarR(v) match {
                case Some(("None", unit)) =>
                    isRecR(unit) match {
                        case Some(Vector()) => Some(Null)
                        case _              => None
                    }
                case _ =>
                    None
            }
        val fs : Seq[ValueR => Option[DbValue]] =
            Seq(tryTrue, tryFalse, tryInteger, tryString, tryNotNull, tryNull)
        fs.foldLeft(None : Option[DbValue]) {
            case (None, f)          => f(value)
            case (out @ Some(_), _) => out
        }
    }

    def rowFromCooma(backend : Backend)(row : backend.ValueR) : Option[Seq[(java.lang.String, DbValue)]] = {
        import backend._
        @tailrec
        def aux(fields : Seq[FldR], out : Seq[(java.lang.String, DbValue)]) : Option[Seq[(java.lang.String, DbValue)]] =
            fields match {
                case hd +: tl =>
                    isFldR(hd) match {
                        case Some((k, v)) =>
                            fromCooma(backend)(v) match {
                                case Some(dbValue) => aux(tl, out :+ (k -> dbValue))
                                case None          => None
                            }
                        case None => None
                    }
                case _ =>
                    Some(out)
            }
        isRecR(row).flatMap(aux(_, Seq.empty))
    }

}
