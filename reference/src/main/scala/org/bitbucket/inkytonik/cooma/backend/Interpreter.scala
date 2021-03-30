/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.cooma
package backend

import org.bitbucket.inkytonik.kiama.output.PrettyPrinter

class Interpreter(config : Config) extends PrettyPrinter {

    self : ReferenceBackend =>

    import org.bitbucket.inkytonik.cooma.CoomaParserSyntax.ASTNode
    import org.bitbucket.inkytonik.cooma.Util.escape
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.{Document, emptyDocument, Width}
    import scala.annotation.tailrec
    import org.bitbucket.inkytonik.kiama.relation.Bridge

    sealed abstract class ValueR
    case class ClsR(source : Bridge[ASTNode], env : Env, f : String, x : String, e : Term) extends ValueR
    case class ErrR(msg : String) extends ValueR
    case class IntR(num : BigInt) extends ValueR
    case class RecR(fields : Vector[FldR]) extends ValueR
    case class StrR(str : String) extends ValueR
    case class VarR(c : String, v : ValueR) extends ValueR
    case class VecR(elems : Vector[ValueR]) extends ValueR

    case class FldR(f : String, x : ValueR)

    case class ClsC(env : Env, k : String, e : Term)

    sealed abstract class Env
    case class ConsCE(source : Bridge[ASTNode], env : Env, x : String, v : ClsC) extends Env
    case class ConsFE(env : Env, clsEnv : () => Env, ds : Vector[DefTerm]) extends Env
    case class ConsVE(source : Bridge[ASTNode], env : Env, x : String, v : ValueR) extends Env
    case class NilE() extends Env

    def consEnv(env : Env, i : String, v : ValueR) : Env =
        ConsVE(null, env, i, v)

    def interpret(term : Term, args : Seq[String], config : Config) : Unit = {
        if (config.server()) {
            if (driver.settingBool("showTrace"))
                driver.publishProduct(source, "trace", "IR")
        }
        interpret(term, emptyEnv, args, config) match {
            case err @ ErrR(msg) =>
                config.output().emitln(showRuntimeValue(err))
                if (config.server()) {
                    if (driver.settingBool("showResult"))
                        driver.publishProduct(source, "result", "cooma", pretty(value(msg)))
                }
            case v =>
                if (config.resultPrint())
                    config.output().emitln(showRuntimeValue(v))
                if (config.server()) {
                    if (driver.settingBool("showResult"))
                        driver.publishProduct(source, "result", "cooma", formatRuntimeValue(v, 5))
                }
        }
    }

    def interpret(term : Term, env : Env, args : Seq[String], config : Config) : ValueR = {

        @tailrec
        def interpretAux(rho : Env, term : Term) : ValueR = {

            if (config.trace()) {
                config.output().emit(showState(rho, term))
            }
            if (config.server()) {
                if (driver.settingBool("showTrace")) {
                    driver.publishProduct(source, "trace", "IR", formatState(rho, term), true)
                }
            }

            term match {
                case AppC(_, "$halt", x) =>
                    if (config.server()) {
                        if (driver.settingBool("showTrace")) {
                            driver.publishProduct(source, "trace", "IR", emptyDocument, true)
                        }
                    }
                    lookupR(rho, x)

                case AppC(source, k, x) =>
                    lookupC(rho, k) match {
                        case ClsC(rho2, y, t) =>
                            interpretAux(ConsVE(source, rho2, y, lookupR(rho, x)), t)

                        case v =>
                            sys.error(s"interpret AppC: $k is $v")
                    }

                case AppF(_, f, k, x) =>
                    lookupR(rho, f) match {
                        case ClsR(source, rho2, j, y, t) =>
                            interpretAux(
                                ConsCE(
                                    source,
                                    ConsVE(source, rho2, y, lookupR(rho, x)),
                                    j,
                                    lookupC(rho, k)
                                ),
                                t
                            )

                        case err : ErrR =>
                            err

                        case v =>
                            sys.error(s"interpret AppF: $f is $v")
                    }

                case CasV(_, x, cs) =>
                    lookupR(rho, x) match {
                        case VarR(c1, v) =>
                            val optSourceK =
                                cs.collectFirst {
                                    case CaseTerm(source, c2, k) if c1 == c2 =>
                                        (source, k)
                                }
                            optSourceK match {
                                case Some((source, k)) =>
                                    lookupC(rho, k) match {
                                        case ClsC(rho2, y, t) =>
                                            interpretAux(ConsVE(source, rho2, y, v), t)

                                        case v =>
                                            sys.error(s"interpret CasV: $k is $v")
                                    }

                                case None =>
                                    sys.error(s"interpret CasV: can't find case for variant $c1")
                            }

                        case err : ErrR =>
                            err

                        case v =>
                            sys.error(s"interpret CasV: $x is $v")
                    }

                case LetC(source, k, x, t1, t2) =>
                    val rho2 = ConsCE(source, rho, k, ClsC(rho, x, t1))
                    interpretAux(rho2, t2)

                case LetF(_, ds, t) =>
                    lazy val rho2 : Env = ConsFE(rho, () => rho2, ds)
                    interpretAux(rho2, t)

                case LetV(source, x, v, t) =>
                    val rho2 : Env = ConsVE(source, rho, x, interpretValue(source, v, rho))
                    interpretAux(rho2, t)
            }

        }

        def interpretValue(source : Bridge[ASTNode], value : Value, rho : Env) : ValueR =
            value match {
                case FunV(k, x, t) =>
                    ClsR(source, rho, k, x, t)

                case IntV(i) =>
                    IntR(i)

                case PrmV(p, xs) =>
                    p.eval(this)(rho, xs, args)

                case RecV(fields) =>
                    RecR(fields.map {
                        case FieldValue(f, v) =>
                            FldR(f, lookupR(rho, v))
                    })

                case StrV(s) =>
                    StrR(s)

                case VarV(c, v) =>
                    VarR(c, lookupR(rho, v))

                case VecV(elems) =>
                    val results = elems.map(s => lookupR(rho, s))
                    results.collectFirst {
                        case e : ErrR => e
                    }.getOrElse(VecR(results))
            }

        interpretAux(env, term)
    }

    @tailrec
    final def lookupR(rho : Env, x : String) : ValueR =
        rho match {
            case ConsCE(_, rho2, _, _) =>
                lookupR(rho2, x)

            case ConsFE(rho2, rho2f, ds) =>
                ds.collectFirst {
                    case DefTerm(source, f, k, y, e) if x == f =>
                        ClsR(source, rho2f(), k, y, e)
                } match {
                    case Some(v) =>
                        v
                    case None =>
                        lookupR(rho2, x)
                }

            case ConsVE(_, rho2, y, v) if x == y =>
                v

            case ConsVE(_, rho2, _, _) =>
                lookupR(rho2, x)

            case NilE() =>
                sys.error(s"lookupR: can't find value $x")
        }

    def lookupC(rho : Env, x : String) : ClsC =
        rho match {
            case ConsCE(_, rho2, y, v) if x == y =>
                v

            case ConsCE(_, rho2, _, _) =>
                lookupC(rho2, x)

            case ConsFE(rho2, _, _) =>
                lookupC(rho2, x)

            case ConsVE(_, rho2, _, _) =>
                lookupC(rho2, x)

            case NilE() =>
                sys.error(s"lookupC: can't find $x")
        }

    /*
	 * Pretty-printer for runtime result values.
	 */

    override val defaultIndent = 2

    def showState(rho : Env, term : Term) : String =
        formatState(rho, term).layout

    def formatState(rho : Env, term : Term) : Document =
        pretty(toDocEnv(rho) <@> toDocTerm(term) <> line)

    def showRuntimeValue(v : ValueR) : String =
        formatRuntimeValue(v).layout

    def formatRuntimeValue(v : ValueR, w : Width = defaultWidth) : Document =
        pretty(group(toDocRuntimeValue(v)), w)

    def showEnv(rho : Env) : String =
        formatEnv(rho, 5).layout

    def formatEnv(rho : Env, w : Width = defaultWidth) : Document =
        pretty(group(toDocEnv(rho)), w)

    def toDocRuntimeValue(v : ValueR) : Doc =
        v match {
            case ClsR(source, v1, v2, v3, v4) =>
                link(
                    source.cross,
                    "<function>"
                )
            case ErrR(msg) =>
                s"cooma: $msg"
            case IntR(i) =>
                value(i)
            case RecR(Vector()) =>
                "{}"
            case RecR(v1) =>
                "{" <> nest(line <> ssep(v1.map(toDocField), "," <> line)) <@> "}"
            case StrR(v1) =>
                "\"" <> value(escape(v1)) <> "\""
            case VarR(c, `unitR`) =>
                c.toLowerCase()
            case VarR(v1, v2) =>
                "<" <+> value(v1) <+> "=" <+> toDocRuntimeValue(v2) <+> ">"
            case VecR(elems) =>
                "[" <> ssep(elems.map(toDocRuntimeValue), ", ") <> "]"
        }

    def toDocField(field : FldR) : Doc =
        value(field.f) <+> "=" <+> toDocRuntimeValue(field.x)

    def toDocEnv(rho : Env) : Doc =
        rho match {
            case ConsCE(source, e, x, ClsC(_, k, body)) =>
                link(
                    source.cross,
                    line <> x <+> k <+> "=" <+> align(toDocTerm(body)) <> toDocEnv(e)
                )

            case ConsFE(e, ce, ds) =>
                hcat(ds.map(toDocDefTerm)) <> toDocEnv(e)

            case ConsVE(source, e, x, v) =>
                link(
                    source.cross,
                    line <> x <+> "=" <+> align(toDocRuntimeValue(v))
                ) <> toDocEnv(e)

            case NilE() =>
                line
        }

}
