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
package reference

trait Interpreter {

    self : ReferenceBackend =>

    import java.nio.file.{Files, Paths}
    import org.bitbucket.inkytonik.cooma.Util.fresh
    import org.bitbucket.inkytonik.cooma.Utils.escape
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinter._
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.{Document, Width}

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

    def emptyEnv : Env =
        NilE()

    def consEnv(env : Env, i : String, v : ValueR) : Env =
        ConsVE(env, i, v)

    def interpret(term : Term, args : Seq[String], config : Config) {
        interpret(term, NilE(), args) match {
            case ErrR(msg) =>
                config.output().emitln(s"cooma: $msg")
            case v =>
                if (config.resultPrint())
                    config.output().emitln(showRuntimeValue(v))
        }
    }

    def interpret(term : Term, env : Env, args : Seq[String]) : ValueR = {

        def interpretAux(rho : Env, term : Term) : ValueR =
            term match {
                case AppC("$halt", x) =>
                    lookupR(rho, x)

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

                case RowV(fields) =>
                    RowR(fields.map {
                        case FieldValue(f, v) =>
                            FldR(f, lookupR(rho, v))
                    })

                case StrV(s) =>
                    StrR(s)
            }

        interpretAux(env, term)
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

    def capability(rho : Env, name : String, x : String) : ValueR = {

        val argument =
            lookupR(rho, x) match {
                case StrR(s) =>
                    s
                case err : ErrR =>
                    return err
                case v =>
                    sys.error(s"interpretPrim console: got non-String $v")
            }

        def makeCapability(field : String, primitive : Primitive) : RowR = {
            val k = fresh("k")
            val y = fresh("y")
            val p = fresh("p")
            RowR(Vector(
                FldR(
                    field,
                    ClsR(NilE(), k, y,
                        LetV(p, PrmV(primitive, Vector(y)),
                            AppC(k, p)))
                )
            ))
        }

        name match {
            case "Console" =>
                if (Files.isWritable(Paths.get(argument)))
                    makeCapability("write", ConsoleWriteP(argument))
                else
                    ErrR(s"Console capability unavailable: can't write $argument")

            case "Reader" =>
                if (Files.isReadable(Paths.get(argument)))
                    makeCapability("read", ReaderReadP(argument))
                else
                    ErrR(s"Reader capability unavailable: can't read $argument")

            case _ =>
                sys.error(s"capability: unknown primitive $name")
        }

    }

    /*
     * Pretty-printer for runtime result values.
     */

    def showRuntimeValue(v : ValueR) : String =
        formatRuntimeValue(v, 5).layout

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

    // Primitives

    abstract class Primitive {
        def numArgs : Int

        def eval(interp : Interpreter)(rho : interp.Env, xs : Seq[String], args : Seq[String]) : interp.ValueR =
            if (xs.length == numArgs)
                run(interp)(rho, xs, args)
            else
                sys.error(s"$show: expected $numArgs arg(s), got $xs")

        def run(interp : Interpreter)(rho : interp.Env, xs : Seq[String], args : Seq[String]) : interp.ValueR

        def show : String
    }

    case class ArgumentP(i : Int) extends Primitive {
        val numArgs = 0

        def run(interp : Interpreter)(rho : interp.Env, xs : Seq[String], args : Seq[String]) : interp.ValueR =
            if ((i < 0) || (i >= args.length))
                interp.ErrR(s"command-line argument $i does not exist (arg count = ${args.length})")
            else
                interp.StrR(args(i))

        def show = s"arg $i"
    }

    case class CapabilityP(cap : String) extends Primitive {
        val numArgs = 1

        def run(interp : Interpreter)(rho : interp.Env, xs : Seq[String], args : Seq[String]) : interp.ValueR =
            interp.capability(rho, cap, xs(0))

        def show = s"cap $cap"
    }

    case class ConsoleWriteP(filename : String) extends Primitive {
        import java.nio.file.{Files, Paths}

        val numArgs = 1

        def run(interp : Interpreter)(rho : interp.Env, xs : Seq[String], args : Seq[String]) : interp.ValueR = {
            val x = xs(0)
            val s = interp.lookupR(rho, x) match {
                case interp.IntR(i) =>
                    i.toString
                case interp.StrR(s) =>
                    s
                case v =>
                    sys.error(s"$show: can't write $v")
            }
            Files.write(Paths.get(filename), s.getBytes())
            interp.RowR(Vector())
        }

        def show = s"consoleWrite $filename"
    }

    case class ReaderReadP(filename : String) extends Primitive {
        import org.bitbucket.inkytonik.kiama.util.FileSource

        val numArgs = 1

        def run(interp : Interpreter)(rho : interp.Env, xs : Seq[String], args : Seq[String]) : interp.ValueR =
            interp.StrR(FileSource(filename).content)

        def show = s"readerRead $filename"
    }

    case class RowConcatP() extends Primitive {
        val numArgs = 2

        def run(interp : Interpreter)(rho : interp.Env, xs : Seq[String], args : Seq[String]) : interp.ValueR = {
            val Vector(l, r) = xs
            interp.lookupR(rho, l) match {
                case interp.RowR(lFields) =>
                    interp.lookupR(rho, r) match {
                        case interp.RowR(rFields) =>
                            interp.RowR(lFields ++ rFields)
                        case rv =>
                            sys.error(s"$show: right argument $r of & is non-row $rv")
                    }
                case lv =>
                    sys.error(s"$show: left argument $l of & is non-row $lv")
            }
        }

        def show = "concat"
    }

    case class RowSelectP() extends Primitive {
        val numArgs = 2

        def run(interp : Interpreter)(rho : interp.Env, xs : Seq[String], args : Seq[String]) : interp.ValueR = {
            val Vector(r, f1) = xs

            interp.lookupR(rho, r) match {
                case interp.RowR(fields) =>
                    fields.collectFirst {
                        case interp.FldR(f2, v) if f1 == f2 =>
                            v
                    } match {
                        case Some(v) =>
                            v
                        case None =>
                            sys.error(s"$show: can't find field $f1 in $fields")
                    }

                case err : interp.ErrR =>
                    err

                case v =>
                    sys.error(s"$show: $r is $v, looking for field $f1")
            }
        }

        def show = "select"
    }

}
