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

sealed abstract class ValueR
case class ClsR(env : Env, f : String, x : String, e : Term) extends ValueR
case class ErrR(msg : String) extends ValueR
case class IntR(num : Int) extends ValueR
case class RowR(fields : Vector[FldR]) extends ValueR
case class StrR(str : String) extends ValueR

case class FldR(x : String, v : ValueR)

case class ClsC(env : Env, k : String, e : Term)

sealed abstract class Env
case class ConsCE(env : Env, x : String, v : ClsC) extends Env
case class ConsFE(env : Env, clsEnv : () => Env, ds : Vector[DefTerm]) extends Env
case class ConsVE(env : Env, x : String, v : ValueR) extends Env
case class NilE() extends Env

object Runtime {

    import org.bitbucket.inkytonik.cooma.Util.escape
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinter._
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.{Document, Width}

    /*
     * Pretty-printer for runtime result values.
     */

    def showRuntimeValue(v : ValueR, w : Width = defaultWidth) : String =
        formatRuntimeValue(v, w).layout

    def formatRuntimeValue(v : ValueR, w : Width = defaultWidth) : Document =
        pretty(group(toDocRuntimeValue(v)), w)

    def toDocRuntimeValue(v : ValueR) : Doc =
        v match {
            case ClsR(v1, v2, v3, v4) =>
                text("<function>")
            case ErrR(msg) =>
                text(msg)
            case IntR(i) =>
                value(i)
            case RowR(v1) =>
                text("{") <> ssep(v1.map(toDocField), text(",") <> space) <> text("}")
            case StrR(v1) =>
                text("\"") <> value(escape(v1)) <> text("\"")
        }

    def toDocField(field : FldR) : Doc =
        value(field.x) <+> text("=") <+> toDocRuntimeValue(field.v)

}
