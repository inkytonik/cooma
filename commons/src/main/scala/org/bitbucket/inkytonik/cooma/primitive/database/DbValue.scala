package org.bitbucket.inkytonik.cooma.primitive.database

import org.bitbucket.inkytonik.cooma.Backend

sealed trait DbValue extends Product {

    def toSql =
        this match {
            case DbValue.Boolean(boolean) => boolean.toString.toUpperCase
            case DbValue.Integer(int)     => int.toString
            case DbValue.String(string)   => s"'$string'"
            case DbValue.NotNull(value)   => value.toString
            case DbValue.Null             => "NULL"
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

}

object DbValue {

    sealed trait Atomic extends DbValue

    case class Boolean(boolean : scala.Boolean) extends Atomic

    case class Integer(int : scala.Int) extends Atomic

    case class String(string : java.lang.String) extends Atomic

    sealed trait Nullable[+A <: Atomic] extends DbValue

    case class NotNull[A <: Atomic](value : A) extends Nullable[A]

    case object Null extends Nullable[Nothing]

}
