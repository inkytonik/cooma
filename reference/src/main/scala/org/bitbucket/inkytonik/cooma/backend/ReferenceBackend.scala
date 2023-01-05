/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019-2023 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.cooma.backend

import org.bitbucket.inkytonik.cooma.{Backend, Config, Driver}
import org.bitbucket.inkytonik.kiama.util.Source

class ReferenceBackend(
    val driver: Driver,
    val source: Source,
    config: Config
) extends Interpreter(config)
    with Backend {

  import java.io.{StringWriter, Writer}
  import org.bitbucket.inkytonik.cooma.CoomaParserSyntax
  import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
  import org.bitbucket.inkytonik.cooma.PrettyPrinter._
  // import scala.collection.mutable

  override def backendName: String = "Reference"

  type Term = CoomaParserSyntax.Term
  type Cont = CoomaParserSyntax.Cont
  type CaseTerm = CoomaParserSyntax.CaseTerm
  type DefTerm = CoomaParserSyntax.DefTerm
  type Value = CoomaParserSyntax.Value
  type FldV = CoomaParserSyntax.FldV

  override type OutputValueR = ValueR

  // Terms

  def appC(k: Cont, x: String): Term =
    AppC(k, x)

  def appF(f: String, k: String, x: String): Term =
    AppF(f, k, x)

  def casV(x: String, cs: Vector[CaseTerm]): Term =
    CasV(x, cs)

  def letC(k: String, x: String, t: Term, body: Term): Term =
    LetC(k, x, t, body)

  def letF(ds: Vector[DefTerm], body: Term): Term =
    LetF(ds, body)

  // Continuations

  def haltC(): Cont =
    HaltC()

  def idnC(k: String): Cont =
    IdnC(k)

  // Values

  def letV(x: String, v: Value, body: Term): Term =
    LetV(x, v, body)

  def caseTerm(c: String, k: String): CaseTerm =
    CaseTerm(c, k)

  def defTerm(f: String, k: String, x: String, body: Term): DefTerm =
    DefTerm(f, k, x, body)

  def funV(k: String, x: String, body: Term): Value =
    FunV(k, x, body)

  def intV(i: BigInt): Value =
    IntV(i)

  def prmV(p: Primitive, xs: Vector[String]): Value =
    PrmV(p, xs)

  def recV(fs: Vector[FldV]): Value =
    RecV(fs)

  def strV(s: String): Value =
    StrV(s)

  def varV(field: FldV): Value =
    VarV(field)

  def vecV(es: Vector[String]): Value =
    VecV(es)

  def fldV(f: String, x: String): FldV =
    FldV(f, x)

  def stdout: Writer =
    new StringWriter() {
      override def write(s: String): Unit = getConfig.output().emit(s)
    }

  // Runtime values

  def strR(str: String): ValueR =
    StrR(str)

  def varR(c: String, v: ValueR): ValueR =
    VarR(FldR(c, v))

  def intR(num: BigInt): ValueR =
    IntR(num)

  def clsR(f: String, x: String, env: Env, e: Term): ValueR =
    ClsR(f, x, env, e)

  def recR(fields: Vector[FldR]): ValueR =
    RecR(fields)

  def vecR(es: Vector[ValueR]): ValueR =
    VecR(es)

  def fldR(x: String, v: ValueR): FldR =
    FldR(x, v)

  def isStrR(value: ValueR): Option[String] =
    value match {
      case StrR(s) => Some(s)
      case _       => None
    }

  def isIntR(value: ValueR): Option[BigInt] =
    value match {
      case IntR(i) => Some(i)
      case _       => None
    }

  def isRecR(value: ValueR): Option[Vector[FldR]] =
    value match {
      case RecR(fields) => Some(fields)
      case _            => None
    }

  def isVarR(value: ValueR): Option[(String, ValueR)] =
    value match {
      case VarR(FldR(c, v)) => Some((c, v))
      case _                => None
    }

  def isVecR(value: ValueR): Option[Vector[ValueR]] =
    value match {
      case VecR(es) => Some(es)
      case _        => None
    }

  def isFldR(value: FldR): Option[(String, ValueR)] =
    value match {
      case FldR(x, v) => Some((x, v))
      case _          => None
    }

  def getFieldName(value: FldR): String =
    value.f

  def getFieldValue(value: FldR): ValueR =
    value.x

  def showTerm(t: Term): String =
    layout(group(toDoc(t)), 5)

  def getConfig: Config = config

  def emptyEnv: Env = NilE()

}
