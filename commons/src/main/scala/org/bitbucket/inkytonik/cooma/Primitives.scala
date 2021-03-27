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

import java.io._
import java.nio.file.Paths

import org.bitbucket.inkytonik.cooma.PrimitiveUtils.readReaderContents
import org.bitbucket.inkytonik.cooma.Primitives._
import org.bitbucket.inkytonik.cooma.Util.fresh
import org.bitbucket.inkytonik.cooma.exceptions.CapabilityException
import scalaj.http.Http

trait Primitives[I <: Backend] {

    self : I =>

    trait Primitive {
        def numArgs : Int

        def eval(rho : Env, xs : Seq[String], args : Seq[String]) : ValueR =
            if (xs.length == numArgs)
                run(rho, xs, args)
            else
                sys.error(s"$show: expected $numArgs arg(s), got $xs")

        def run(rho : Env, xs : Seq[String], args : Seq[String]) : ValueR

        def show : String
    }

    case class ArgumentP(i : Int) extends Primitive {
        val numArgs = 0

        def run(rho : Env, xs : Seq[String], args : Seq[String]) : ValueR =
            if ((i < 0) || (i >= args.length))
                errR(s"command-line argument $i does not exist (arg count = ${args.length})")
            else
                strR(args(i))

        def show = s"arg $i"
    }

    case class ReaderReadP(filename : String) extends Primitive {

        import org.bitbucket.inkytonik.cooma.PrimitiveUtils.readReaderContents

        val numArgs = 1

        if (CoomaConstants.CONSOLEIO != filename && !PrimitiveUtils.isFileReadable(filename))
            throw new CapabilityException(s"Reader capability unavailable: can't read $filename")

        def run(rho : Env, xs : Seq[String], args : Seq[String]) : ValueR = {

            lazy val in : BufferedReader =
                new BufferedReader(filename match {
                    case CoomaConstants.CONSOLEIO => new InputStreamReader(System.in)
                    case _                        => new BufferedReader(new FileReader(filename))
                })

            try {
                strR(readReaderContents(in))
            } catch {
                case e : IOException => sys.error(e.getMessage)
            }
        }

        def show = s"readerRead $filename"
    }

    case class CapabilityP(cap : String) extends Primitive {
        val numArgs = 1

        def run(rho : Env, xs : Seq[String], args : Seq[String]) : ValueR =
            capability(rho, cap, xs.head)

        def capability(rho : Env, name : String, x : String) : ValueR = {

            def makeCapability(pairs : Vector[(String, Primitive, Int)]) : ValueR = {
                recR(pairs.map {
                    case (fieldName, primitive, numArgs) =>
                        val p = fresh("p")
                        def aux(numArgs : Int, args : Vector[String], k0 : String) : Term = {
                            val k = fresh("k")
                            val y = fresh("y")
                            if (numArgs > 0)
                                letV(null, p, funV(k, y, aux(numArgs - 1, args :+ y, k)),
                                    appC(null, k0, p))
                            else
                                letV(null, p, prmV(primitive, args), appC(null, k0, p))
                        }
                        val k = fresh("k")
                        val y = fresh("y")
                        fldR(fieldName, clsR(
                            null, emptyEnv, k, y,
                            aux(numArgs - 1, Vector(y), k)
                        ))
                })
            }

            val value = lookupR(rho, x)
            val argument = isStrR(value) match {
                case Some(s) => s
                case None => isErrR(value) match {
                    case Some(_) => return value
                    case None    => sys.error(s"$show: got non-String argument $value")
                }
            }

            name match {
                case "HttpDelete" | "HttpGet" | "HttpPost" | "HttpPut" =>
                    val method = name.substring(4)
                    try {
                        makeCapability(Vector((
                            method.toLowerCase,
                            httpClientP(method.toUpperCase, argument), 1
                        )))
                    } catch {
                        case capE : CapabilityException => errR(capE.getMessage)
                    }
                case "FolderReader" =>
                    try {
                        makeCapability(Vector(("read", folderReaderReadP(argument), 1)))
                    } catch {
                        case capE : CapabilityException => errR(capE.getMessage)
                    }
                case "FolderWriter" =>
                    try {
                        makeCapability(Vector(("write", folderWriterWriteP(argument), 2)))
                    } catch {
                        case capE : CapabilityException => errR(capE.getMessage)
                    }
                case "Reader" =>
                    try {
                        makeCapability(Vector(("read", readerReadP(argument), 1)))
                    } catch {
                        case capE : CapabilityException => errR(capE.getMessage)
                    }
                case "Writer" =>
                    try {
                        makeCapability(Vector(("write", writerWriteP(argument), 1)))
                    } catch {
                        case capE : CapabilityException => errR(capE.getMessage)
                    }
                case _ =>
                    sys.error(s"capability: unknown primitive $name")
            }
        }

        def show = s"cap $cap"
    }

    trait FolderP extends Primitive {
        def root : String

        override def run(rho : Env, xs : Seq[String], args : Seq[String]) : ValueR =
            xs.toList match {
                case suffixIdn :: tl =>
                    val suffix = lookupR(rho, suffixIdn)
                    val filename =
                        isStrR(suffix)
                            .map(suffix => s"$root/$suffix")
                            .getOrElse(sys.error(s"$show: expected String, got $suffix"))
                    if (Paths.get(filename).normalize.startsWith(Paths.get(root).normalize))
                        handleFile(rho, new File(filename), tl)
                    else
                        errR(s"$show: $filename is not a descendant of $root")
                case Nil =>
                    sys.error(s"$show: folder primitives require at least one argument")
            }

        def handleFile(rho : Env, file : File, xs : Seq[String]) : ValueR
    }

    case class FolderReaderReadP(root : String) extends FolderP {
        val numArgs = 1

        override def handleFile(rho : Env, file : File, xs : Seq[String]) : ValueR = {
            val in = new BufferedReader(new FileReader(file))
            try {
                strR(readReaderContents(in))
            } catch {
                case e : IOException => sys.error(e.getMessage)
            }
        }

        def show = s"folderReaderRead $root"
    }

    case class FolderWriterWriteP(root : String) extends FolderP {
        val numArgs = 2

        override def handleFile(rho : Env, file : File, xs : Seq[String]) : ValueR = {
            val text = {
                val text = lookupR(rho, xs.head)
                isStrR(text).getOrElse(sys.error(s"$show: can't write $text"))
            }
            val out = new BufferedWriter(new FileWriter(file))
            try {
                out.write(text)
            } finally {
                out.close()
            }
            recR(Vector())
        }

        def show = s"folderWriterWrite $root"
    }

    case class RecConcatP() extends Primitive {
        val numArgs = 2

        def run(rho : Env, xs : Seq[String], args : Seq[String]) : ValueR =
            xs match {
                case Vector(l, r) =>
                    val vl = lookupR(rho, l)
                    val vr = lookupR(rho, r)
                    def aux(v : ValueR, side : String) : Either[String, Vector[FldR]] =
                        (isRecR(v), isErrR(v)) match {
                            case (Some(fields), _) => Right(fields)
                            case (_, Some(error))  => Left(error)
                            case _                 => sys.error(s"$show: $side argument $r of & is non-record")
                        }
                    (aux(vl, "left"), aux(vr, "right")) match {
                        case (Right(lFields), Right(rFields)) =>
                            recR(lFields ++ rFields)
                        case (l, r) =>
                            errR(Seq(l, r).flatMap(_.swap.toOption).mkString(", "))
                    }
                case _ =>
                    sys.error(s"$show: unexpectedly got arguments $xs")
            }

        def show = "concat"
    }

    case class WriterWriteP(filename : String, stdout : Writer) extends Primitive {
        val numArgs = 1

        if (CoomaConstants.CONSOLEIO != filename &&
            !PrimitiveUtils.isFileWritable(filename)) throw new CapabilityException(s"Writer capability unavailable: can't write $filename")

        def run(rho : Env, xs : Seq[String], args : Seq[String]) : ValueR = {

            lazy val out : Writer = filename match {
                case CoomaConstants.CONSOLEIO => stdout
                case _                        => new BufferedWriter(new FileWriter(filename))
            }

            val value = lookupR(rho, xs.head)
            val s = isIntR(value) match {
                case Some(i : BigInt) => i.toString
                case None => isStrR(value) match {
                    case Some(s) => s
                    case None    => sys.error(s"$show: can't write $value")
                }
            }
            try {
                out.write(s)
            } finally {
                out.close()
            }
            recR(Vector())
        }

        def show = s"consoleWrite $filename"

    }

    case class HttpClientP(
        method : String,
        url : String
    ) extends Primitive {
        val numArgs = 1

        def run(rho : Env, xs : Seq[String], args : Seq[String]) : ValueR = {
            val x = xs.head
            isStrR(lookupR(rho, x)) match {
                case Some(suffix) =>
                    val (code, body) = {
                        val response = Http(url + suffix).method(method).asString
                        (response.code, response.body)
                    }
                    recR(Vector(
                        fldR("code", intR(code)),
                        fldR("body", strR(body))
                    ))
                case None =>
                    sys.error(s"$show: can't find string operand $x")
            }
        }

        def show = s"httpClient ($method): $url"
    }

    case class RecSelectP() extends Primitive {
        val numArgs = 2

        def run(rho : Env, xs : Seq[String], args : Seq[String]) : ValueR =
            xs match {
                case Vector(r, f1) =>
                    val value = lookupR(rho, r)
                    isRecR(value) match {
                        case Some(fields) =>
                            fields.collectFirst {
                                case f if isFldR(f).isDefined && isFldR(f).get._1 == f1 =>
                                    isFldR(f).get._2
                            } match {
                                case Some(v) => v
                                case None    => sys.error(s"$show: can't find field $f1 in $fields")
                            }
                        case None => isErrR(value) match {
                            case Some(_) => value
                            case None    => sys.error(s"$show: $r is $value, looking for field $f1")
                        }
                    }
                case _ =>
                    sys.error(s"$show: unexpectedly got arguments $xs")
            }

        def show = "select"
    }

    case class EqualP() extends Primitive {
        val numArgs = 3

        def run(rho : Env, xs : Seq[String], args : Seq[String]) : ValueR = {

            def getField(f : String, r : Vector[FldR]) : Option[FldR] =
                r.find(getFieldName(_) == f)

            def equal(lvalue : ValueR, rvalue : ValueR) : Boolean = {
                (isIntR(lvalue), isIntR(rvalue)) match {
                    case (Some(li), Some(ri)) =>
                        li == ri
                    case _ =>
                        (isStrR(lvalue), isStrR(rvalue)) match {
                            case (Some(ls), Some(rs)) =>
                                ls == rs
                            case _ =>
                                (isRecR(lvalue), isRecR(rvalue)) match {
                                    case (Some(lfs), Some(rfs)) =>
                                        lfs.forall(lfld => {
                                            val f = getFieldName(lfld)
                                            getField(f, rfs) match {
                                                case Some(rfld) =>
                                                    equal(getFieldValue(lfld), getFieldValue(rfld))
                                                case None =>
                                                    sys.error(s"equal: can't find field $f in $rfs")
                                            }
                                        })
                                    case _ =>
                                        (isVarR(lvalue), isVarR(rvalue)) match {
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
                    if (equal(lookupR(rho, l), lookupR(rho, r)))
                        trueR
                    else
                        falseR
                case _ =>
                    sys.error(s"$show: unexpectedly got arguments $xs")
            }

        }

        def show = "equal"
    }

    abstract class IntPrimitive extends Primitive {
        def run(rho : Env, xs : Seq[String], args : Seq[String]) : ValueR = {
            val operands = xs.map(s => isIntR(lookupR(rho, s)) match {
                case Some(v) => v
                case _       => sys.error(s"$show: can't find integer operand $s")
            })
            doRun(operands)
        }

        def doRun(operands : Seq[BigInt]) : ValueR
    }

    case class IntBinOp(op : IntPrimBinOp) extends IntPrimitive {
        val numArgs = op.numArgs
        def show = op.primName

        def doRun(operands : Seq[BigInt]) : ValueR = {
            try {
                (op, operands) match {
                    case (ABS, Vector(i))    => intR(i.abs)
                    case (ADD, Vector(l, r)) => intR(l + r)
                    case (DIV, Vector(l, r)) => intR(l / r)
                    case (MUL, Vector(l, r)) => intR(l * r)
                    case (POW, Vector(l, r)) =>
                        if (r < 0)
                            errR(s"$show: illegal negative power $r given")
                        else
                            intR(l.pow(r.toInt))
                    case (SUB, Vector(l, r)) => intR(l - r)
                    case _ =>
                        sys.error(s"$show $op: unexpectedly got operands $operands")
                }
            } catch {
                case e : Throwable =>
                    errR(s"Error executing integer ${op.name}: ${e.getMessage}")
            }
        }
    }

    case class IntRelOp(op : IntPrimRelOp) extends IntPrimitive {
        val numArgs = op.numArgs
        def show = op.primName

        def doRun(operands : Seq[BigInt]) : ValueR =
            operands match {
                case Vector(left, right) =>
                    if (op match {
                        case GT  => left > right
                        case GTE => left >= right
                        case LT  => left < right
                        case LTE => left <= right
                    }) {
                        trueR
                    } else {
                        falseR
                    }
                case _ =>
                    sys.error(s"$show $op: unexpectedly got operands $operands")
            }
    }

    case class StringPrimitive(op : StrPrimOp) extends Primitive {
        val numArgs = op.numArgs
        def show = op.primName

        def run(rho : Env, xs : Seq[String], args : Seq[String]) : ValueR = {

            def extractStrParam(x : String) : String =
                isStrR(lookupR(rho, x)) match {
                    case Some(v) => v
                    case _       => sys.error(s"$show: can't find string operand $x")
                }

            def extractIntParam(x : String) : BigInt =
                isIntR(lookupR(rho, x)) match {
                    case Some(v) => v
                    case _       => sys.error(s"$show: can't find integer operand $x")
                }

            (op, xs) match {
                case (CONCAT, Vector(lx, rx)) =>
                    val l = extractStrParam(lx)
                    val r = extractStrParam(rx)
                    strR(l + r)

                case (LENGTH, Vector(sx)) =>
                    val s = extractStrParam(sx)
                    intR(s.length)

                case (SUBSTR, Vector(sx, ix)) => {
                    val s = extractStrParam(sx)
                    val i = extractIntParam(ix)
                    if ((i < 0) || (i > s.length))
                        errR(s"""$show: index $i out of range for string "$s"""")
                    else
                        strR(s.substring(i.toInt))
                }

                case _ =>
                    sys.error(s"$show $op: unexpectedly got arguments $xs")
            }
        }
    }

}

object Primitives {

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

    sealed abstract class IntPrimRelOp extends Product with IntPrimOp {
        val numArgs = 2
    }
    case object GT extends IntPrimRelOp
    case object GTE extends IntPrimRelOp
    case object LT extends IntPrimRelOp
    case object LTE extends IntPrimRelOp

    val allIntPrimRelOps = Vector(GT, GTE, LT, LTE)

    sealed abstract class StrPrimOp(val numArgs : Int) extends Product with PrimOp {
        val prefix = "Str"
    }
    case object CONCAT extends StrPrimOp(2)
    case object LENGTH extends StrPrimOp(1)
    case object SUBSTR extends StrPrimOp(2)

    val allStrPrimOps = Vector(CONCAT, LENGTH, SUBSTR)

}
