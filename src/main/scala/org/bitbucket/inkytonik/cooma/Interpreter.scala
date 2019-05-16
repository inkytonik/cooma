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

class Interpreter(config : Config) {

    import java.nio.file.{Files, Paths}
    import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
    import org.bitbucket.inkytonik.cooma.Util.{escape, fresh}
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinter._
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.{Document, Width}
    import org.bitbucket.inkytonik.kiama.util.FileSource

    // Runtime structures

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

    def interpret(term : Term, args : List[String]) {

        val unit = RowR(Vector())

        def interpretAux(rho : Env, term : Term) : ValueR =
            term match {
                case AppC(k, x) =>
                    lookupC(rho, k) match {
                        case ClsC(rho2, y, t) =>
                            interpretAux(ConsVE(rho, y, lookupR(rho, x)), t)

                        case v =>
                            sys.error(s"interpret AppC: $k is $v")
                    }

                case AppF(f, k, x) =>
                    lookupR(rho, f) match {
                        case ClsR(rho2, j, y, t) =>
                            interpretAux(
                                ConsCE(
                                    ConsVE(rho, y, lookupR(rho, x)),
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

                case Halt(x) =>
                    lookupR(rho, x)

                case LetC(k, x, t1, t2) =>
                    val rho2 = ConsCE(rho, k, ClsC(rho, x, t1))
                    interpretAux(rho2, t2)

                case LetF(ds, t) =>
                    lazy val rho2 : Env = ConsFE(rho, () => rho2, ds)
                    interpretAux(rho2, t)

                case LetV(x, v, t) =>
                    val rho2 = ConsVE(rho, x, interpretValue(v, rho))
                    interpretAux(rho2, t)
            }

        def interpretValue(value : Value, rho : Env) : ValueR =
            value match {
                case AndV(l, r) =>
                    lookupR(rho, l) match {
                        case RowR(lFields) =>
                            lookupR(rho, r) match {
                                case RowR(rFields) =>
                                    RowR(lFields ++ rFields)
                                case rv =>
                                    sys.error(s"interpretValue: right argument $r of & is non-row $rv")
                            }
                        case lv =>
                            sys.error(s"interpretValue: left argument $r of & is non-row $lv")
                    }

                case ArgV(i) =>
                    if ((i < 0) || (i >= args.length))
                        ErrR(s"command-line argument $i does not exist (arg count = ${args.length})")
                    else
                        StrR(args(i))

                case CapV(p, x) =>
                    interpretCap(p, x, rho)

                case FunV(k, x, t) =>
                    ClsR(rho, k, x, t)

                case IntV(i) =>
                    IntR(i)

                case PrmV(name, args) =>
                    (name, args) match {
                        case ("console", Vector(f, x)) =>
                            val s =
                                lookupR(rho, x) match {
                                    case IntR(i) =>
                                        i.toString
                                    case StrR(s) =>
                                        s
                                    case v =>
                                        sys.error(s"interpretValue: can't write $v")
                                }
                            Files.write(Paths.get(f), s.getBytes())
                            unit

                        case ("reader", Vector(f)) =>
                            StrR(FileSource(f).content)

                        case (name, arg) =>
                            sys.error(s"interpretValue: unknown primitive call $name $arg")
                    }

                case RowV(fields) =>
                    RowR(fields.map {
                        case FieldValue(f, v) =>
                            FldR(f, lookupR(rho, v))
                    })

                case SelV(x, f1) =>
                    lookupR(rho, x) match {
                        case RowR(fields) =>
                            fields.collectFirst {
                                case FldR(f2, v) if f1 == f2 =>
                                    v
                            } match {
                                case Some(v) =>
                                    v
                                case None =>
                                    sys.error(s"interpret SelV: can't find field $f1 in $fields")
                            }

                        case err : ErrR =>
                            err

                        case v =>
                            sys.error(s"interpret SelV: $x is $v, looking for field $f1")
                    }

                case StrV(s) =>
                    StrR(s)
            }

        def interpretCap(name : String, x : String, rho : Env) : ValueR =
            name match {
                case "Console" =>
                    console(x, rho)
                case "Reader" =>
                    reader(x, rho)
                case _ =>
                    sys.error(s"interpretCap: unknown primitive $name")
            }

        val initEnv =
            ConsCE(
                NilE(),
                "halt",
                ClsC(NilE(), "x", Halt("x"))
            )

        interpretAux(initEnv, term) match {
            case ErrR(msg) =>
                config.output().emitln(s"cooma: $msg")
            case v =>
                if (config.resultPrint())
                    config.output().emitln(showRuntimeValue(v))
        }
    }

    def console(x : String, rho : Env) : ValueR =
        lookupR(rho, x) match {
            case StrR(s) =>
                if (Files.isWritable(Paths.get(s))) {
                    val k = fresh("k")
                    val y = fresh("y")
                    val p = fresh("p")
                    RowR(Vector(
                        FldR(
                            "write",
                            ClsR(NilE(), k, y,
                                LetV(p, PrmV("console", Vector(s, y)),
                                    AppC(k, p)))
                        )
                    ))
                } else
                    ErrR(s"Console capability unavailable: can't write $s")

            case err : ErrR =>
                err

            case v =>
                sys.error(s"interpretPrim console: got non-String $v")
        }

    def reader(x : String, rho : Env) : ValueR =
        lookupR(rho, x) match {
            case StrR(s) =>
                if (Files.isReadable(Paths.get(s))) {
                    val k = fresh("k")
                    val y = fresh("y")
                    val p = fresh("p")
                    RowR(Vector(
                        FldR(
                            "read",
                            ClsR(NilE(), k, y,
                                LetV(p, PrmV("reader", Vector(s)),
                                    AppC(k, p)))
                        )
                    ))
                } else
                    ErrR(s"Reader capability unavailable: can't read $s")

            case err : ErrR =>
                err

            case v =>
                sys.error(s"interpretPrim reader: got non-String $v")
        }

    def lookupR(rho : Env, x : String) : ValueR =
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
        value(field.x) <> space <> text("=") <> space <> toDocRuntimeValue(field.v)

}
