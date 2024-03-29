/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019-2023 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.cooma
package backend

import java.io.File
import java.io.BufferedReader
import java.io.InputStreamReader
import org.bitbucket.inkytonik.kiama.util.StringSource
import java.util.stream.Collectors

class Interpreter(config: Config) {

  self: ReferenceBackend =>

  import org.bitbucket.inkytonik.cooma.CoomaException._
  import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
  import org.bitbucket.inkytonik.cooma.PrettyPrinter._
  import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.{
    Document,
    Width,
    emptyDocument
  }
  import org.bitbucket.inkytonik.kiama.util.{FileSource, Messaging, Positions}

  import scala.annotation.tailrec
  import scala.util.Try

  sealed abstract class ValueR
  case class ClsR(f: String, x: String, env: Env, e: Term) extends ValueR
  case class IntR(num: BigInt) extends ValueR
  case class RecR(fields: Vector[FldR]) extends ValueR
  case class StrR(str: String) extends ValueR
  case class VarR(field: FldR) extends ValueR
  case class VecR(elems: Vector[ValueR]) extends ValueR

  case class FldR(f: String, x: ValueR)

  case class ClsC(k: String, env: Env, e: Term)

  sealed abstract class Env
  case class ConsCE(x: String, v: ClsC, env: Env) extends Env
  case class ConsFE(ds: Vector[DefTerm], env: Env) extends Env
  case class ConsVE(x: String, v: ValueR, env: Env) extends Env
  case class NilE() extends Env

  def interpret(term: Term, args: Seq[String], config: Config): Unit = {
    if (config.server() && driver.settingBool("showTrace"))
      driver.publishProduct(source, "trace", "IR")
    preludeDynamicEnv(config).flatMap(
      interpret(term, _, args, config).map(_.value)
    ) match {
      case Right(result) =>
        if (config.resultPrint())
          config.output().emitln(showRuntimeValue(result))
        if (config.server() && driver.settingBool("showResult"))
          driver.publishProduct(
            source,
            "result",
            "cooma",
            formatRuntimeValue(result, 5)
          )
      case Left(msg) =>
        config.output().emitln(msg)
        if (config.server() && driver.settingBool("showResult"))
          driver.publishProduct(source, "result", "cooma", pretty(value(msg)))
    }
  }

  def interpret(
      term: Term,
      rho: Env,
      args: Seq[String],
      config: Config
  ): Either[String, Result] = {

    @tailrec
    def interpretAux(rho: Env, term: Term): Result = {

      if (config.trace())
        config.output().emit(showState(rho, term))
      if (config.server())
        if (driver.settingBool("showTrace"))
          driver.publishProduct(
            source,
            "trace",
            "IR",
            formatState(rho, term),
            true
          )

      term match {
        case AppC(HaltC(), x) =>
          if (config.server()) {
            if (driver.settingBool("showTrace")) {
              driver.publishProduct(source, "trace", "IR", emptyDocument, true)
            }
          }
          Result(rho, lookupR(rho, x))

        case AppC(IdnC(k), x) =>
          lookupC(rho, k) match {
            case ClsC(y, rho2, t) =>
              interpretAux(ConsVE(y, lookupR(rho, x), rho2), t)

            case v =>
              errInterp("AppC", s"$k is $v")
          }

        case AppF(f, k, x) =>
          lookupR(rho, f) match {
            case ClsR(j, y, rho2, t) =>
              interpretAux(
                ConsCE(
                  j,
                  lookupC(rho, k),
                  ConsVE(y, lookupR(rho, x), rho2)
                ),
                t
              )

            case v =>
              errInterp("AppF", s"$f is $v")
          }

        case CasV(x, cs) =>
          lookupR(rho, x) match {
            case VarR(FldR(c1, v)) =>
              val optSourceK =
                cs.collectFirst {
                  case ct @ CaseTerm(c2, k) if c2 == c1 =>
                    (ct, k)
                }
              optSourceK match {
                case Some((ct, k)) =>
                  lookupC(rho, k) match {
                    case ClsC(y, rho2, t) =>
                      val rho3 = ConsVE(y, v, rho2)
                      interpretAux(rho3, t)

                    case v =>
                      errInterp("CasV", s"$k is $v")
                  }

                case None =>
                  errInterp("CasV", s"can't find case for variant $c1")
              }

            case v =>
              errInterp("CasV", s"$x is $v")
          }

        case LetC(k, x, t1, t2) =>
          val rho2 = ConsCE(k, ClsC(x, rho, t1), rho)
          interpretAux(rho2, t2)

        case LetF(ds, t) =>
          val rho2 = ConsFE(ds, rho)
          interpretAux(rho2, t)

        case LetV(x, v, t) =>
          val rho2: Env = ConsVE(x, interpretValue(v, rho), rho)
          interpretAux(rho2, t)
      }

    }

    def interpretValue(value: Value, rho: Env): ValueR =
      value match {
        case FunV(k, x, t) =>
          ClsR(k, x, rho, t)

        case IntV(i) =>
          IntR(i)

        case PrmV(p, xs) =>
          evalPrim(p, rho, xs, args)

        case RecV(fields) =>
          RecR(fields.map { case FldV(f, v) =>
            FldR(f, lookupR(rho, v))
          })

        case StrV(s) =>
          StrR(s)

        case VarV(FldV(c, v)) =>
          VarR(FldR(c, lookupR(rho, v)))

        case VecV(elems) =>
          VecR(elems.map(s => lookupR(rho, s)))
      }

    Try(interpretAux(rho, term)).toEither.left.map {
      case e: CoomaException => e.toString
      case e                 => getUnhandledMessage(e)
    }
  }

  @tailrec
  final def lookupR(rho: Env, x: String): ValueR =
    rho match {
      case ConsCE(_, _, rho2) =>
        lookupR(rho2, x)

      case ConsFE(ds, rho2) =>
        ds.collectFirst {
          case DefTerm(f, k, y, e) if x == f =>
            ClsR(k, y, rho, e)
        } match {
          case Some(v) =>
            v
          case None =>
            lookupR(rho2, x)
        }

      case ConsVE(y, v, _) if x == y =>
        v

      case ConsVE(_, _, rho2) =>
        lookupR(rho2, x)

      case NilE() =>
        errInterp("lookupR", s"can't find value $x")
    }

  def insertR(rho: Env, x: String, value: ValueR): Env =
    ConsVE(x, value, rho)

  def lookupC(rho: Env, x: String): ClsC =
    rho match {
      case ConsCE(y, v, _) if x == y =>
        v

      case ConsCE(_, _, rho2) =>
        lookupC(rho2, x)

      case ConsFE(_, rho2) =>
        lookupC(rho2, x)

      case ConsVE(_, _, rho2) =>
        lookupC(rho2, x)

      case NilE() =>
        errInterp("lookupC", s"can't find $x")
    }

  def preludeDynamicEnv(config: Config): Either[String, Env] =
    if (config.noPrelude() || config.compilePrelude())
      Right(emptyEnv)
    else
      Try(
        readDynamicPrelude(s"${config.preludePath()}.dynamic", config)
      ).toEither.left.map {
        case e: CoomaException => e.message
        case e                 => getUnhandledMessage(e)
      }

  def readDynamicPrelude(filename: String, config: Config): Env = {
    val source =
      if ((new File(filename)).isFile)
        FileSource(filename)
      else {
        val stream = getClass.getClassLoader.getResourceAsStream(filename)
        val br = new BufferedReader(new InputStreamReader(stream))
        val text = br.lines().collect(Collectors.joining("\n"))
        stream.close()
        StringSource(text)
      }
    val positions = new Positions()
    val p = new CoomaParser(source, positions)
    val pr = p.pDynamicPrelude(0)
    if (pr.hasValue) {
      val prelude = p.value(pr).asInstanceOf[DynamicPrelude]
      val preludeConfig = new Config(Seq("-Q", filename))
      preludeConfig.verify()
      interpret(prelude.term, emptyEnv, Seq(), preludeConfig).left
        .map(errPrelude)
        .merge
        .rho
    } else {
      config.output().emitln(s"cooma: can't parse dynamic prelude '$filename'")
      val message = p.errorToMessage(pr.parseError)
      val messaging = new Messaging(positions)
      messaging.report(source, Vector(message), config.output())
      sys.exit(1)
    }
  }

  def showState(rho: Env, term: Term): String =
    formatState(rho, term).layout

  def formatState(rho: Env, term: Term): Document =
    pretty(toDocEnv(rho) <@> toDoc(term) <> line)

  def showRuntimeValue(v: ValueR): String =
    formatRuntimeValue(v).layout

  def formatRuntimeValue(v: ValueR, w: Width = defaultWidth): Document =
    pretty(group(toDocRuntimeValue(v)), w)

  def showEnv(rho: Env): String =
    formatEnv(rho, 5).layout

  def formatEnv(rho: Env, w: Width = defaultWidth): Document =
    pretty(group(toDocEnv(rho)), w)

  def toDocRuntimeValue(v: ValueR): Doc =
    v match {
      case ClsR(v1, v2, v3, v4) =>
        "<function>"
      case IntR(i) =>
        value(i)
      case RecR(Vector()) =>
        "{}"
      case RecR(v1) =>
        "{" <> nest(line <> ssep(v1.map(toDocField), "," <> line)) <@> "}"
      case StrR(v1) =>
        "\"" <> value(v1) <> "\""
      case VarR(FldR(v1, v2)) =>
        "<<" <+> value(v1) <+> "=" <+> toDocRuntimeValue(v2) <+> ">>"
      case VecR(elems) =>
        "[" <> ssep(elems.map(toDocRuntimeValue), ", ") <> "]"
    }

  def toDocField(field: FldR): Doc =
    value(field.f) <+> "=" <+> toDocRuntimeValue(field.x)

  def toDocEnv(rho: Env): Doc =
    rho match {
      case ConsCE(x, ClsC(k, _, body), e) =>
        line <> x <+> k <+> "=" <+> align(toDoc(body)) <> toDocEnv(e)
      case ConsFE(ds, e) =>
        hcat(ds.map(toDoc)) <> toDocEnv(e)
      case ConsVE(x, v, e) =>
        line <> x <+> "=" <+> align(toDocRuntimeValue(v)) <> toDocEnv(e)
      case NilE() =>
        line
    }

}
