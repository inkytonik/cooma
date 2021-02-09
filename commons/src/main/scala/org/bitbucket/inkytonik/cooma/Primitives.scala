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

import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter, IOException, InputStreamReader, Writer}

import org.bitbucket.inkytonik.cooma.Util.fresh
import org.bitbucket.inkytonik.cooma.exceptions.CapabilityException

object Primitives {

    trait Primitive[I <: Backend] {
        def numArgs : Int

        def eval(interp : I)(rho : interp.Env, xs : Seq[String], args : Seq[String]) : interp.ValueR =
            if (xs.length == numArgs)
                run(interp)(rho, xs, args)
            else
                sys.error(s"$show: expected $numArgs arg(s), got $xs")

        def run(interp : I)(rho : interp.Env, xs : Seq[String], args : Seq[String]) : interp.ValueR

        def show : String
    }

    case class ArgumentP[I <: Backend](i : Int) extends Primitive[I] {
        val numArgs = 0

        def run(interp : I)(rho : interp.Env, xs : Seq[String], args : Seq[String]) : interp.ValueR =
            if ((i < 0) || (i >= args.length))
                interp.errR(s"command-line argument $i does not exist (arg count = ${args.length})")
            else
                interp.strR(args(i))

        def show = s"arg $i"
    }

    case class ReaderReadP[I <: Backend](filename : String) extends Primitive[I] {

        import org.bitbucket.inkytonik.cooma.PrimitiveUtils.readReaderContents

        val numArgs = 1

        if (CoomaConstants.CONSOLEIO != filename && !PrimitiveUtils.isFileReadable(filename))
            throw new CapabilityException(s"Reader capability unavailable: can't read $filename")

        def run(interp : I)(rho : interp.Env, xs : Seq[String], args : Seq[String]) : interp.ValueR = {

            lazy val in : BufferedReader =
                new BufferedReader(filename match {
                    case CoomaConstants.CONSOLEIO => new InputStreamReader(System.in)
                    case _                        => new BufferedReader(new FileReader(filename))
                })

            try {
                interp.strR(readReaderContents(in))
            } catch {
                case e : IOException => sys.error(e.getMessage)
            }
        }

        def show = s"readerRead $filename"
    }

    case class CapabilityP[I <: Backend](cap : String) extends Primitive[I] {
        val numArgs = 1

        def run(interp : I)(rho : interp.Env, xs : Seq[String], args : Seq[String]) : interp.ValueR =
            capability(interp)(rho, cap, xs.head)

        def capability(interp : I)(rho : interp.Env, name : String, x : String) : interp.ValueR = {

            def makeCapability(pairs : Vector[(String, interp.Primitive)]) : interp.ValueR = {
                interp.recR(
                    pairs.map(pair => {
                        val k = fresh("k")
                        val y = fresh("y")
                        val p = fresh("p")
                        interp.fldR(
                            pair._1, interp.clsR(
                                null,
                                interp.emptyEnv, k, y,
                                interp.letV(null, p, interp.prmV(pair._2, Vector(y)),
                                    interp.appC(null, k, p))
                            )
                        )
                    })
                )
            }

            val value = interp.lookupR(rho, x)
            val argument = interp.isStrR(value) match {
                case Some(s) => s
                case None => interp.isErrR(value) match {
                    case Some(_) => return value
                    case None    => sys.error(s"$show: got non-String argument $value")
                }
            }

            name match {
                case "Writer" =>
                    try {
                        makeCapability(Vector(("write", interp.writerWriteP(argument))))
                    } catch {
                        case capE : CapabilityException => interp.errR(capE.getMessage)
                    }
                case "Reader" =>
                    try {
                        makeCapability(Vector(("read", interp.readerReadP(argument))))
                    } catch {
                        case capE : CapabilityException => interp.errR(capE.getMessage)
                    }
                case "ReaderWriter" =>
                    try {
                        makeCapability(Vector(
                            ("read", interp.readerReadP(argument)),
                            ("write", interp.writerWriteP(argument))
                        ))
                    } catch {
                        case capE : CapabilityException => interp.errR(capE.getMessage)
                    }
                case _ =>
                    sys.error(s"capability: unknown primitive $name")
            }
        }

        def show = s"cap $cap"
    }

    case class RecConcatP[I <: Backend]() extends Primitive[I] {
        val numArgs = 2

        def run(interp : I)(rho : interp.Env, xs : Seq[String], args : Seq[String]) : interp.ValueR =
            xs match {
                case Vector(l, r) =>
                    interp.isRecR(interp.lookupR(rho, l)) match {
                        case Some(lFields) =>
                            interp.isRecR(interp.lookupR(rho, r)) match {
                                case Some(rFields) => interp.recR(lFields ++ rFields)
                                case None          => sys.error(s"$show: right argument $r of & is non-record")
                            }
                        case None => sys.error(s"$show: left argument $l of & is non-record")
                    }
                case _ =>
                    sys.error(s"$show: unexpectedly got arguments $xs")
            }

        def show = "concat"
    }

    case class WriterWriteP[I <: Backend](filename : String, stdout : Writer) extends Primitive[I] {
        val numArgs = 1

        if (CoomaConstants.CONSOLEIO != filename &&
            !PrimitiveUtils.isFileWritable(filename)) throw new CapabilityException(s"Writer capability unavailable: can't write $filename")

        def run(interp : I)(rho : interp.Env, xs : Seq[String], args : Seq[String]) : interp.ValueR = {

            lazy val out : Writer = filename match {
                case CoomaConstants.CONSOLEIO => stdout
                case _                        => new BufferedWriter(new FileWriter(filename))
            }

            val value = interp.lookupR(rho, xs.head)
            val s = interp.isIntR(value) match {
                case Some(i : BigInt) => i.toString
                case None => interp.isStrR(value) match {
                    case Some(s) => s
                    case None    => sys.error(s"$show: can't write $value")
                }
            }
            try {
                out.write(s)
            } finally {
                out.close()
            }
            interp.recR(Vector())
        }

        def show = s"consoleWrite $filename"

    }

    case class RecSelectP[I <: Backend]() extends Primitive[I] {
        val numArgs = 2

        def run(interp : I)(rho : interp.Env, xs : Seq[String], args : Seq[String]) : interp.ValueR =
            xs match {
                case Vector(r, f1) =>
                    val value = interp.lookupR(rho, r)
                    interp.isRecR(value) match {
                        case Some(fields) =>
                            fields.collectFirst {
                                case f if interp.isFldR(f).isDefined && interp.isFldR(f).get._1 == f1 =>
                                    interp.isFldR(f).get._2
                            } match {
                                case Some(v) => v
                                case None    => sys.error(s"$show: can't find field $f1 in $fields")
                            }
                        case None => interp.isErrR(value) match {
                            case Some(_) => value
                            case None    => sys.error(s"$show: $r is $value, looking for field $f1")
                        }
                    }
                case _ =>
                    sys.error(s"$show: unexpectedly got arguments $xs")
            }

        def show = "select"
    }

    case class EqualP[I <: Backend]() extends Primitive[I] {
        val numArgs = 3

        def run(interp : I)(rho : interp.Env, xs : Seq[String], args : Seq[String]) : interp.ValueR = {

            def getField(f : String, r : Vector[interp.FldR]) : Option[interp.FldR] =
                r.find(interp.getFieldName(_) == f)

            def equal(lvalue : interp.ValueR, rvalue : interp.ValueR) : Boolean = {
                (interp.isIntR(lvalue), interp.isIntR(rvalue)) match {
                    case (Some(li), Some(ri)) =>
                        li == ri
                    case _ =>
                        (interp.isStrR(lvalue), interp.isStrR(rvalue)) match {
                            case (Some(ls), Some(rs)) =>
                                ls == rs
                            case _ =>
                                (interp.isRecR(lvalue), interp.isRecR(rvalue)) match {
                                    case (Some(lfs), Some(rfs)) =>
                                        lfs.forall(lfld => {
                                            val f = interp.getFieldName(lfld)
                                            getField(f, rfs) match {
                                                case Some(rfld) =>
                                                    equal(interp.getFieldValue(lfld), interp.getFieldValue(rfld))
                                                case None =>
                                                    sys.error(s"equal: can't find field $f in $rfs")
                                            }
                                        })
                                    case _ =>
                                        (interp.isVarR(lvalue), interp.isVarR(rvalue)) match {
                                            case (Some((lc, lv)), Some((rc, rv))) =>
                                                (lc == rc) && (equal(lv, rv))
                                            case _ =>
                                                false
                                        }
                                }
                        }
                }
            }

            xs match {
                case Vector(_, l, r) =>
                    if (equal(interp.lookupR(rho, l), interp.lookupR(rho, r)))
                        interp.trueR
                    else
                        interp.falseR
                case _ =>
                    sys.error(s"$show: unexpectedly got arguments $xs")
            }

        }

        def show = "equal"
    }

    abstract class IntPrimitive[I <: Backend] extends Primitive[I] {
        def run(interp : I)(rho : interp.Env, xs : Seq[String], args : Seq[String]) : interp.ValueR = {
            val operands = xs.map(s => interp.isIntR(interp.lookupR(rho, s)) match {
                case Some(v) => v
                case _       => sys.error(s"$show: can't find integer operand $s")
            })
            doRun(interp)(operands)
        }

        def doRun(interp : I)(operands : Seq[BigInt]) : interp.ValueR
    }

    trait PrimOp {
        self : Product =>
        def numArgs : Int
        def prefix : String
        val name = productPrefix.toLowerCase
        val camelName = s"${name.head.toUpper}${name.tail}"
        lazy val primName = s"$prefix$camelName"
    }

    trait IntPrimOp extends PrimOp with Product {
        val prefix = "Int"
    }

    sealed abstract class IntPrimUnOp extends Product with IntPrimOp {
        val numArgs = 1
    }

    sealed abstract class IntPrimBinOp extends Product with IntPrimOp {
        val numArgs = 2
    }

    case object ABS extends IntPrimBinOp {
        override val numArgs = 1
    }
    case object ADD extends IntPrimBinOp
    case object SUB extends IntPrimBinOp
    case object MUL extends IntPrimBinOp
    case object DIV extends IntPrimBinOp
    case object POW extends IntPrimBinOp

    val allInt1PrimBinOps = Vector(ABS)
    val allInt2PrimBinOps = Vector(ADD, SUB, MUL, DIV, POW)

    case class IntBinOp[I <: Backend](op : IntPrimBinOp) extends IntPrimitive[I] {
        val numArgs = op.numArgs
        def show = op.primName

        def doRun(interp : I)(operands : Seq[BigInt]) : interp.ValueR = {
            try {
                (op, operands) match {
                    case (ABS, Vector(i))    => interp.intR(i.abs)
                    case (ADD, Vector(l, r)) => interp.intR(l + r)
                    case (DIV, Vector(l, r)) => interp.intR(l / r)
                    case (MUL, Vector(l, r)) => interp.intR(l * r)
                    case (POW, Vector(l, r)) =>
                        if (r < 0)
                            interp.errR(s"$show: illegal negative power $r given")
                        else
                            interp.intR(l.pow(r.toInt))
                    case (SUB, Vector(l, r)) => interp.intR(l - r)
                    case _ =>
                        sys.error(s"$show $op: unexpectedly got operands $operands")
                }
            } catch {
                case e : Throwable =>
                    interp.errR(s"Error executing integer ${op.name}: ${e.getMessage}")
            }
        }
    }

    sealed abstract class IntPrimRelOp extends Product with IntPrimOp {
        val numArgs = 2
    }
    case object GT extends IntPrimRelOp
    case object GTE extends IntPrimRelOp
    case object LT extends IntPrimRelOp
    case object LTE extends IntPrimRelOp

    val allIntPrimRelOps = Vector(GT, GTE, LT, LTE)

    case class IntRelOp[I <: Backend](op : IntPrimRelOp) extends IntPrimitive[I] {
        val numArgs = op.numArgs
        def show = op.primName

        def doRun(interp : I)(operands : Seq[BigInt]) : interp.ValueR =
            operands match {
                case Vector(left, right) =>
                    if (op match {
                        case GT  => left > right
                        case GTE => left >= right
                        case LT  => left < right
                        case LTE => left <= right
                    }) {
                        interp.trueR
                    } else {
                        interp.falseR
                    }
                case _ =>
                    sys.error(s"$show $op: unexpectedly got operands $operands")
            }
    }

    sealed abstract class StrPrimOp(val numArgs : Int) extends Product with PrimOp {
        val prefix = "Str"
    }
    case object CONCAT extends StrPrimOp(2)
    case object LENGTH extends StrPrimOp(1)
    case object SUBSTR extends StrPrimOp(2)

    val allStrPrimOps = Vector(CONCAT, LENGTH, SUBSTR)

    case class StringPrimitive[I <: Backend](op : StrPrimOp) extends Primitive[I] {
        val numArgs = op.numArgs
        def show = op.primName

        def run(interp : I)(rho : interp.Env, xs : Seq[String], args : Seq[String]) : interp.ValueR = {

            def extractStrParam(x : String) : String =
                interp.isStrR(interp.lookupR(rho, x)) match {
                    case Some(v) => v
                    case _       => sys.error(s"$show: can't find string operand $x")
                }

            def extractIntParam(x : String) : BigInt =
                interp.isIntR(interp.lookupR(rho, x)) match {
                    case Some(v) => v
                    case _       => sys.error(s"$show: can't find integer operand $x")
                }

            (op, xs) match {
                case (CONCAT, Vector(lx, rx)) =>
                    val l = extractStrParam(lx)
                    val r = extractStrParam(rx)
                    interp.strR(l + r)

                case (LENGTH, Vector(sx)) =>
                    val s = extractStrParam(sx)
                    interp.intR(s.length)

                case (SUBSTR, Vector(sx, ix)) => {
                    val s = extractStrParam(sx)
                    val i = extractIntParam(ix)
                    if ((i < 0) || (i > s.length))
                        interp.errR(s"""$show: index $i out of range for string "$s"""")
                    else
                        interp.strR(s.substring(i.toInt))
                }

                case _ =>
                    sys.error(s"$show $op: unexpectedly got arguments $xs")
            }
        }
    }

}
