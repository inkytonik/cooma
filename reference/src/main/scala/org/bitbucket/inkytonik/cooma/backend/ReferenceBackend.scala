/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.cooma.backend
import java.io.StringWriter

import org.bitbucket.inkytonik.cooma.{Backend, Config, Driver, Primitives}
import org.bitbucket.inkytonik.kiama.util.Source

class ReferenceBackend(
    val driver : Driver,
    val source : Source,
    config : Config
) extends Interpreter(config) with Backend {

    import org.bitbucket.inkytonik.cooma.Util.escape
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinter._
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.{Document, Width}
    import scala.annotation.tailrec

    override def backendName : String = "Reference"

    type Primitive = org.bitbucket.inkytonik.cooma.Primitives.Primitive[ReferenceBackend]

    sealed abstract class Value
    case class FunV(k : String, x : String, body : Term) extends Value
    case class IntV(i : BigInt) extends Value
    case class PrmV(p : Primitive, xs : Vector[String]) extends Value
    case class RecV(fs : Vector[FieldValue]) extends Value
    case class StrV(s : String) extends Value
    case class VarV(v : String, x : String) extends Value

    case class FieldValue(f : String, x : String)

    sealed abstract class Term
    case class AppC(k : String, x : String) extends Term
    case class AppF(f : String, k : String, x : String) extends Term
    case class CasV(x : String, ks : Vector[CaseTerm]) extends Term
    case class LetC(k : String, x : String, t : Term, body : Term) extends Term
    case class LetF(ds : Vector[DefTerm], body : Term) extends Term
    case class LetV(x : String, v : Value, body : Term) extends Term

    case class CaseTerm(c : String, k : String)
    case class DefTerm(f : String, k : String, x : String, body : Term)

    override type OutputValueR = ValueR

    // Terms

    def appC(k : String, x : String) : Term =
        AppC(k, x)

    def appF(f : String, k : String, x : String) : Term =
        AppF(f, k, x)

    def casV(x : String, cs : Vector[CaseTerm]) : Term =
        CasV(x, cs)

    def letC(k : String, x : String, t : Term, body : Term) : Term =
        LetC(k, x, t, body)

    def letF(ds : Vector[DefTerm], body : Term) : Term =
        LetF(ds, body)

    // Values

    def letV(x : String, v : Value, body : Term) : Term =
        LetV(x, v, body)

    def caseTerm(c : String, k : String) : CaseTerm =
        CaseTerm(c, k)

    def defTerm(f : String, k : String, x : String, body : Term) : DefTerm =
        DefTerm(f, k, x, body)

    def funV(k : String, x : String, body : Term) : Value =
        FunV(k, x, body)

    def intV(i : BigInt) : Value =
        IntV(i)

    def prmV(p : Primitive, xs : Vector[String]) : Value =
        PrmV(p, xs)

    def recV(fs : Vector[FieldValue]) : Value =
        RecV(fs)

    def strV(s : String) : Value =
        StrV(s)

    def varV(v : String, x : String) : Value =
        VarV(v, x)

    def fieldValue(f : String, x : String) : FieldValue =
        FieldValue(f, x)

    // Primitives
    import org.bitbucket.inkytonik.cooma.Primitives._
    def argumentP(i : Int) : Primitive =
        ArgumentP(i)

    def capabilityP(cap : String) : Primitive =
        CapabilityP(cap)

    def consoleWriteP(filename : String) : Primitive = {
        val stdout = new StringWriter() {
            override def write(s : String) : Unit = getConfig.output().emit(s)
        }
        WriterWriteP(filename, stdout)
    }

    def readerReadP(filename : String) : Primitive =
        ReaderReadP(filename)

    def recConcatP() : Primitive =
        RecConcatP()

    def recSelectP() : Primitive =
        RecSelectP()

    def intBinP(op : Primitives.IntPrimBinOp.IntPrimBinOp) : Primitive =
        Primitives.IntBinOpP(op)

    def intRelP(op : Primitives.IntPrimRelOp.IntPrimRelOp) : Primitive =
        Primitives.IntRelOp(op)

    def stringP(op : Primitives.StrPrimOp.StrPrimOp) : Primitive =
        Primitives.StringPrimitive(op)

    //Value runtimes
    def errR(msg : String) : ValueR =
        ErrR(msg)

    def strR(str : String) : ValueR =
        StrR(str)

    def varR(c : String, v : ValueR) : ValueR =
        VarR(c, v)

    def intR(num : BigInt) : ValueR =
        IntR(num)

    def clsR(env : Env, f : String, x : String, e : Term) : ValueR =
        ClsR(env, f, x, e)

    def recR(fields : Vector[FldR]) : ValueR =
        RecR(fields)

    def fldR(x : String, v : ValueR) : FldR =
        FldR(x, v)

    def isErrR(value : ValueR) : Option[String] =
        value match {
            case ErrR(s) => Some(s)
            case _       => None
        }

    def isStrR(value : ValueR) : Option[String] =
        value match {
            case StrR(s) => Some(s)
            case _       => None
        }

    def isIntR(value : ValueR) : Option[BigInt] =
        value match {
            case IntR(i) => Some(i)
            case _       => None
        }

    def isFldR(value : FldR) : Option[(String, ValueR)] =
        value match {
            case FldR(x, v) => Some((x, v))
            case _          => None
        }

    def isRecR(value : ValueR) : Option[Vector[FldR]] =
        value match {
            case RecR(fields) => Some(fields)
            case _            => None
        }

    /*
     * Custom IR pretty-printer that escapes string terms.
     */
    def showTerm(t : Term) : String =
        formatTerm(t, 5).layout

    def formatTerm(t : Term, w : Width = defaultWidth) : Document =
        pretty(group(toDocTerm(t)), w)

    def toDocTerm(t : Term) : Doc =
        t match {
            case AppC(k, x) =>
                k <+> x
            case AppF(f, k, x) =>
                f <+> k <+> x
            case CasV(x, ks) =>
                "case" <+> value(x) <+> ssep(ks.map(toDocCaseTerm), space)
            case LetC(k, x, t, body) =>
                "letc" <+> value(k) <+> value(x) <+> "=" <+> align(toDocTerm(t)) <@>
                    toDocTerm(body)
            case v @ LetF(ds, body) =>
                "letf" <> nest(ssep(ds.map(toDocDefTerm), emptyDoc)) <@>
                    toDocTerm(body)
            case LetV(x, v, body) =>
                "letv" <+> value(x) <+> "=" <+> align(toDocValue(v)) <@>
                    toDocTerm(body)
        }

    def toDocCaseTerm(caseTerm : CaseTerm) : Doc =
        '(' <> value(caseTerm.c) <+> value(caseTerm.k) <> ')'

    def toDocDefTerm(defTerm : DefTerm) : Doc =
        line <> value(defTerm.f) <+> value(defTerm.k) <+> value(defTerm.x) <+>
            text("=") <+> align(toDocTerm(defTerm.body))

    def toDocValue(v : Value) : Doc =
        v match {
            case FunV(k, x, t) =>
                "fun" <+> k <+> x <+> text("=") <+> align(toDocTerm(t))
            case IntV(i) =>
                value(i)
            case PrmV(p, xs) =>
                p.show <> hcat(xs.map(x => space <> x))
            case RecV(fs) =>
                "{" <> ssep(fs.map(toDocFieldValue), "," <> space) <> text("}")
            case StrV(v1) =>
                "\"" <> value(escape(v1)) <> text("\"")
            case VarV(v1, v2) =>
                "<" <> value(v1) <+> "=" <+> value(v2) <> text(">")
        }

    def toDocFieldValue(field : FieldValue) : Doc =
        value(field.f) <+> text("=") <+> value(field.x)

    @tailrec
    final def lookupR(rho : Env, x : String) : ValueR =
        rho match {
            case ConsCE(rho2, _, _) =>
                lookupR(rho2, x)

            case ConsFE(rho2, rho2f, ds) =>
                ds.collectFirst {
                    case DefTerm(f, k, y, e) if x == f =>
                        ClsR(rho2f(), k, y, e)
                } match {
                    case Some(v) =>
                        v
                    case None =>
                        lookupR(rho2, x)
                }

            case ConsVE(rho2, y, v) if x == y =>
                v

            case ConsVE(rho2, _, _) =>
                lookupR(rho2, x)

            case NilE() =>
                sys.error(s"lookupR: can't find value $x")
        }

    def getConfig : Config = config

    def emptyEnv : Env = NilE()

}
