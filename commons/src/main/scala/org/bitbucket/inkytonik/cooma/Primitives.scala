package org.bitbucket.inkytonik.cooma

import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter, IOException, InputStreamReader, Writer}

import org.bitbucket.inkytonik.cooma.Primitives.IntPrimRelOp.{EQ, GT, GTE, LT, LTE, NEQ}
import org.bitbucket.inkytonik.cooma.Primitives.StrPrimOp.{CONCAT, LENGTH, SUBSTR}
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
                                interp.emptyEnv, k, y,
                                interp.letV(p, interp.prmV(pair._2, Vector(y)),
                                    interp.appC(k, p))
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
                    case None    => sys.error(s"interpretPrim: got non-String argument $value")
                }
            }

            name match {
                case "Writer" =>
                    try {
                        makeCapability(Vector(("write", interp.consoleWriteP(argument))))
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
                            ("write", interp.consoleWriteP(argument))
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

        def run(interp : I)(rho : interp.Env, xs : Seq[String], args : Seq[String]) : interp.ValueR = {
            val Vector(l, r) = xs
            interp.isRecR(interp.lookupR(rho, l)) match {
                case Some(lFields) =>
                    interp.isRecR(interp.lookupR(rho, r)) match {
                        case Some(rFields) => interp.recR(lFields ++ rFields)
                        case None          => sys.error(s"$show: right argument $r of & is non-record")
                    }
                case None => sys.error(s"$show: left argument $l of & is non-record")
            }
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

        def run(interp : I)(rho : interp.Env, xs : Seq[String], args : Seq[String]) : interp.ValueR = {
            val Vector(r, f1) = xs
            val value = interp.lookupR(rho, r)
            interp.isRecR(value) match {
                case Some(fields) =>
                    //fields.find
                    fields.collectFirst {
                        case f if interp.isFldR(f).isDefined && interp.isFldR(f).get._1 == f1 => interp.isFldR(f).get._2
                    } match {
                        case Some(v) => v
                        case None    => sys.error(s"$show: can't find field $f1 in $fields")
                    }
                case None => interp.isErrR(value) match {
                    case Some(_) => value
                    case None    => sys.error(s"$show: $r is $value, looking for field $f1")
                }
            }
        }

        def show = "select"
    }

    abstract class IntPrimitive[I <: Backend] extends Primitive[I] {
        def run(interp : I)(rho : interp.Env, xs : Seq[String], args : Seq[String]) : interp.ValueR = {
            val operands = xs.map(s => interp.isIntR(interp.lookupR(rho, s)) match {
                case Some(v) => v
                case _       => sys.error(s"IntBinOpP.run: can't find operand $s on environment.")
            })
            doRun(interp)(operands)
        }

        def doRun(interp : I)(operands : Seq[BigInt]) : interp.ValueR
    }

    object IntPrimBinOp extends Enumeration {
        type IntPrimBinOp = Value
        val ADD, SUB, MUL, DIV, POW = Value
    }

    case class IntBinOpP[I <: Backend](op : IntPrimBinOp.IntPrimBinOp) extends IntPrimitive[I] {
        val numArgs = 2

        def doRun(interp : I)(operands : Seq[BigInt]) : interp.ValueR = {
            try {
                interp.intR(op match {
                    case IntPrimBinOp.ADD => operands.sum
                    case IntPrimBinOp.SUB => operands.reduce(_ - _)
                    case IntPrimBinOp.MUL => operands.product
                    case IntPrimBinOp.DIV => operands.reduce(_ / _)
                    case IntPrimBinOp.POW => operands.reduce((x, y) => x.pow(y.toInt))
                })
            } catch {
                case e : Throwable => interp.errR(s"Error executing primitive: ${e.getMessage}")
            }
        }

        def show = "intPrmBinOp"
    }

    object IntPrimRelOp extends Enumeration {
        type IntPrimRelOp = Value
        val EQ, NEQ, GT, GTE, LT, LTE = Value
    }

    case class IntRelOp[I <: Backend](op : IntPrimRelOp.IntPrimRelOp) extends IntPrimitive[I] {
        val numArgs = 2

        def doRun(interp : I)(operands : Seq[BigInt]) : interp.ValueR = {
            val Vector(left, right) = operands
            if (op match {
                case EQ  => left == right
                case NEQ => left != right
                case GT  => left > right
                case GTE => left >= right
                case LT  => left < right
                case LTE => left <= right
            }) {
                interp.trueR()
            } else {
                interp.falseR()
            }
        }

        def show = "intPrmRelOp"
    }

    object StrPrimOp extends Enumeration {
        type StrPrimOp = Value
        val LENGTH, CONCAT, SUBSTR, EQ = Value
    }

    case class StringPrimitive[I <: Backend](op : StrPrimOp.StrPrimOp) extends Primitive[I] {
        val numArgs = 2

        def show = "strPrim"

        override def eval(interp : I)(rho : interp.Env, xs : Seq[String], args : Seq[String]) : interp.ValueR =
            if ((op match {
                case LENGTH       => 1
                case CONCAT       => 2
                case SUBSTR       => 2
                case StrPrimOp.EQ => 2
            }) == xs.length)
                run(interp)(rho, xs, args)
            else
                sys.error(s"$show: expected $numArgs arg(s), got $xs")

        def run(interp : I)(rho : interp.Env, xs : Seq[String], args : Seq[String]) : interp.ValueR = {

            def extractStrParams = {
                xs.map(s => interp.isStrR(interp.lookupR(rho, s)) match {
                    case Some(v) => v
                    case _       => sys.error(s"String Primitive.${op.toString.toLowerCase}: can't find operand $s on environment.")
                })
            }

            op match {
                case LENGTH =>
                    interp.intR(extractStrParams.head.length)
                case CONCAT =>
                    interp.strR(extractStrParams.reduce(_ + _))
                case SUBSTR => {
                    val left = interp.isStrR((interp.lookupR(rho, xs.head))) match {
                        case Some(v) => v
                        case _       => sys.error(s"String Primitive.substr: can't find operand ${xs.head} on environment.")
                    }

                    val right = interp.isIntR((interp.lookupR(rho, xs.tail.head))) match {
                        case Some(v) => v
                        case _       => sys.error(s"String Primitive.substr: can't find operand ${xs.tail.head} on environment.")
                    }
                    interp.strR(left.substring(right.toInt))
                }
                case StrPrimOp.EQ => {
                    val Vector(left, right) = extractStrParams
                    if (left.equals(right)) {
                        interp.trueR()
                    } else {
                        interp.falseR()
                    }
                }
            }
        }
    }

    def generateDynamicRuntime[I <: Backend](interp : I) : Map[String, interp.ValueR] = {
        def generateField(opName : String, p : interp.Primitive, numArgs : Int) : interp.FldR = {
            val closurek = fresh("primk")
            val letvk = fresh("primk")
            val x = fresh("primx")
            val f = fresh("primf")
            val j = fresh("primj")
            val y = fresh("primy")

            if (numArgs == 1) {
                val letvx = fresh("primx")
                interp.fldR(
                    opName,
                    interp.clsR(
                        interp.emptyEnv, closurek, x,
                        interp.letV(
                            letvx,
                            interp.prmV(p, Vector(x)),
                            interp.appC(closurek, letvx)
                        )
                    )
                )
            } else {
                //Two arguments
                interp.fldR(
                    opName,
                    interp.clsR(
                        interp.emptyEnv, closurek, x,
                        interp.letV(f, interp.funV(j, y,
                            interp.letV(
                                letvk,
                                interp.prmV(p, Vector(x, y)),
                                interp.appC(j, letvk)
                            )),
                            interp.appC(closurek, f))
                    )
                )
            }
        }

        Map(
            "Ints" -> interp.recR(
                IntPrimBinOp.values.map(op => { generateField(op.toString.toLowerCase(), interp.intBinP(op), 2) }).toVector ++
                    IntPrimRelOp.values.map(op => { generateField(op.toString.toLowerCase(), interp.intRelP(op), 2) }).toVector
            ),
            "Strings" -> interp.recR(Vector(
                generateField(StrPrimOp.LENGTH.toString.toLowerCase(), interp.stringP(StrPrimOp.LENGTH), 1),
                generateField(StrPrimOp.CONCAT.toString.toLowerCase(), interp.stringP(StrPrimOp.CONCAT), 2),
                generateField(StrPrimOp.EQ.toString.toLowerCase(), interp.stringP(StrPrimOp.EQ), 2),
                generateField(StrPrimOp.SUBSTR.toString.toLowerCase(), interp.stringP(StrPrimOp.SUBSTR), 2)
            ))
        )

    }
}

