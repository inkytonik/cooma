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

import org.bitbucket.inkytonik.kiama.output.{PrettyPrinter => PP}

object PrettyPrinter extends PP {

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

    def showValue(v : ValueR, w : Width = defaultWidth) : String =
        formatValue(v, w).layout

    def formatValue(v : ValueR, w : Width = defaultWidth) : Document =
        pretty(group(toDocValue(v)), w)

    def toDocValue(v : ValueR) : Doc =
        v match {
            case ClsR(v1, v2, v3, v4) =>
                text("<function>")
            case ErrR(v1) =>
                text("err") <> space <> value(v1)
            case IntR(v1) =>
                value(v1)
            case RowR(v1) =>
                text("{") <> ssep(v1.map(toDocField), text(",") <> space) <> text("}")
            case StrR(v1) =>
                text("\"") <> value(escape(v1)) <> text("\"")
        }

    def toDocField(field : FldR) : Doc =
        value(field.identifier) <> space <> text("=") <> space <>
            toDocValue(field.valueR)

}
