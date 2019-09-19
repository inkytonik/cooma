/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.cooma
package backend

class ReferenceBackend(config : Config) extends Interpreter(config) with Backend {

    import org.bitbucket.inkytonik.cooma.Util.escape
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinter._
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.{Document, Width}

    override def backendName : String = "Reference"

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

    def argumentP(i : Int) : Primitive =
        ArgumentP(i)

    def capabilityP(cap : String) : Primitive =
        CapabilityP(cap)

    def consoleWriteP(filename : String) : Primitive =
        WriterWriteP(filename)

    def readerReadP(filename : String) : Primitive =
        ReaderReadP(filename)

    def recConcatP() : Primitive =
        RecConcatP()

    def recSelectP() : Primitive =
        RecSelectP()

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

}
