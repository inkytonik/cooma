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

class Interpreter(config : Config) {

    self : ReferenceBackend =>

    import org.bitbucket.inkytonik.cooma.Util.escape
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinter._
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.{Document, Width}
    import scala.annotation.tailrec

    sealed abstract class ValueR
    case class ClsR(env : Env, f : String, x : String, e : Term) extends ValueR
    case class ErrR(msg : String) extends ValueR
    case class IntR(num : BigInt) extends ValueR
    case class RecR(fields : Vector[FldR]) extends ValueR
    case class StrR(str : String) extends ValueR
    case class VarR(c : String, v : ValueR) extends ValueR

    case class FldR(x : String, v : ValueR)

    case class ClsC(env : Env, k : String, e : Term)

    sealed abstract class Env
    case class ConsCE(env : Env, x : String, v : ClsC) extends Env
    case class ConsFE(env : Env, clsEnv : () => Env, ds : Vector[DefTerm]) extends Env
    case class ConsVE(env : Env, x : String, v : ValueR) extends Env
    case class NilE() extends Env

    def consEnv(env : Env, i : String, v : ValueR) : Env =
        ConsVE(env, i, v)

    def interpret(term : Term, args : Seq[String], config : Config) : Unit = {
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
        def interpretAux(rho : Env, term : Term) : ValueR =
            term match {
                case AppC("$halt", x) =>
                    lookupR(rho, x)

                case AppC(k, x) =>
                    lookupC(rho, k) match {
                        case ClsC(rho2, y, t) =>
                            interpretAux(ConsVE(rho2, y, lookupR(rho, x)), t)

                        case v =>
                            sys.error(s"interpret AppC: $k is $v")
                    }

                case AppF(f, k, x) =>
                    lookupR(rho, f) match {
                        case ClsR(rho2, j, y, t) =>
                            interpretAux(
                                ConsCE(
                                    ConsVE(rho2, y, lookupR(rho, x)),
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

                case CasV(x, cs) =>
                    lookupR(rho, x) match {
                        case VarR(c1, v) =>
                            val optK =
                                cs.collectFirst {
                                    case CaseTerm(c2, k) if c1 == c2 =>
                                        k
                                }
                            optK match {
                                case Some(k) =>
                                    lookupC(rho, k) match {
                                        case ClsC(rho2, y, t) =>
                                            interpretAux(ConsVE(rho2, y, v), t)

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

                case LetC(k, x, t1, t2) =>
                    val rho2 = ConsCE(rho, k, ClsC(rho, x, t1))
                    interpretAux(rho2, t2)

                case LetF(ds, t) =>
                    lazy val rho2 : Env = ConsFE(rho, () => rho2, ds)
                    interpretAux(rho2, t)

                case LetV(x, v, t) =>
                    val rho2 : Env = ConsVE(rho, x, interpretValue(v, rho))
                    interpretAux(rho2, t)
            }

        def interpretValue(value : Value, rho : Env) : ValueR =
            value match {
                case FunV(k, x, t) =>
                    ClsR(rho, k, x, t)

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
            }

        interpretAux(env, term)
    }

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

    def lookupC(rho : Env, x : String) : ClsC =
        rho match {
            case ConsCE(rho2, y, v) if x == y =>
                v

            case ConsCE(rho2, _, _) =>
                lookupC(rho2, x)

            case ConsFE(rho2, _, _) =>
                lookupC(rho2, x)

            case ConsVE(rho2, _, _) =>
                lookupC(rho2, x)

            case NilE() =>
                sys.error(s"lookupC: can't find $x")
        }

    /*
	 * Pretty-printer for runtime result values.
	 */

    def showRuntimeValue(v : ValueR) : String =
        formatRuntimeValue(v).layout

    def formatRuntimeValue(v : ValueR, w : Width = defaultWidth) : Document =
        pretty(group(toDocRuntimeValue(v)), w)

    def toDocRuntimeValue(v : ValueR) : Doc =
        v match {
            case ClsR(v1, v2, v3, v4) =>
                "<function>"
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
        }

    def toDocField(field : FldR) : Doc =
        value(field.x) <+> text("=") <+> toDocRuntimeValue(field.v)

}
