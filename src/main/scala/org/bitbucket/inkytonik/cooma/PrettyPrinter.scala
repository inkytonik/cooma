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

object PrettyPrinter extends syntax.CoomaParserPrettyPrinter {

    import syntax.CoomaParserSyntax._
    import org.bitbucket.inkytonik.cooma.Util.escape
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.{Document, Width}

    /*
     * Custom pretty-printer for runtime result values. Differs from
     * standard one in that it prints closures and strings in a more
     * human-readable way:
     *   - closures as "<function>"
     *   - strings quoted with special characters escaped
     */

    def showRuntimeValue(v : ValueR, w : Width = defaultWidth) : String =
        formatRuntimeValue(v, w).layout

    def formatRuntimeValue(v : ValueR, w : Width = defaultWidth) : Document =
        pretty(group(toDocRuntimeValue(v)), w)

    def toDocRuntimeValue(v : ValueR) : Doc =
        v match {
            case ClsR(v1, v2, v3, v4) =>
                text("<function>")
            case RowR(v1) =>
                text("{") <> ssep(v1.map(toDocField), text(",") <> space) <> text("}")
            case StrR(v1) =>
                text("\"") <> value(escape(v1)) <> text("\"")
            case _ =>
                toDoc(v)
        }

    def toDocField(field : FldR) : Doc =
        value(field.identifier) <> space <> text("=") <> space <>
            toDocRuntimeValue(field.valueR)

    /*
     * Similarly to showRuntimeValue etc but for the IR.
     */
    def showTerm(t : Term, w : Width = defaultWidth) : String =
        formatTerm(t, w).layout

    def formatTerm(t : Term, w : Width = defaultWidth) : Document =
        pretty(group(toDocTerm(t)), w)

    def toDocTerm(t : Term) : Doc =
        t match {
            case LetV(v1, v2, v3) =>
                text("letv") <> space <> value(v1) <> space <> text("=") <> space <> toDocValue(v2) <> space <> text("in") <> space <> nest(line <> toDocTerm(v3))
            case LetC(v1, v2, v3, v4) =>
                text("letc") <> space <> value(v1) <> space <> value(v2) <> space <> text("=") <> space <> toDocTerm(v3) <> space <> text("in") <> space <> nest(line <> toDocTerm(v4))
            case _ =>
                toDoc(t)
        }

    def toDocValue(v : Value) : Doc =
        v match {
            case FunV(v1, v2, v3) =>
                text("fun") <> space <> value(v1) <> space <> value(v2) <> space <> text("=>") <> space <> toDocTerm(v3)
            case RowV(v1) =>
                text("{") <> ssep(v1.map(toDocFieldValue), text(",") <> space) <> text("}")
            case StrV(v1) =>
                text("\"") <> value(escape(v1)) <> text("\"")
            case _ =>
                toDoc(v)
        }

    def toDocFieldValue(field : FieldValue) : Doc =
        value(field.identifier1) <> space <> text("=") <> space <>
            value(field.identifier2)

}
