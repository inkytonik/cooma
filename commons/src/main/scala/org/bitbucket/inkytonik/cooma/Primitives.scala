package org.bitbucket.inkytonik.cooma

import java.io._

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

            lazy val in : Reader =
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

    object IntPrimOp extends Enumeration {
        type IntPrimOp = Value
        val ADD, SUB, MUL, DIV, POW = Value
    }

    case class IntBinOpP[I <: Backend](op : IntPrimOp.IntPrimOp) extends Primitive[I] {
        val numArgs = 2

        def run(interp : I)(rho : interp.Env, xs : Seq[String], args : Seq[String]) : interp.ValueR = {
            val operands = xs.map(s => interp.isIntR(interp.lookupR(rho, s)) match {
                case Some(v) => v
                case _       => sys.error(s"IntBinOpP.run: can't find operand $s on environment.")
            })

            try {
                interp.intR(op match {
                    case IntPrimOp.ADD => operands.sum
                    case IntPrimOp.SUB => operands.reduce(_ - _)
                    case IntPrimOp.MUL => operands.product
                    case IntPrimOp.DIV => operands.reduce(_ / _)
                    case IntPrimOp.POW => operands.reduce((x, y) => x.pow(y.toInt))
                })
            } catch {
                case e : Throwable => interp.errR(s"Error executing primitive: ${e.getMessage}")
            }
        }

        def show = "intPrm"
    }

    def generateDynamicRuntime[I <: Backend](interp : I) : interp.ValueR = {
        //rename, fs for functions, js and ks for continuations
        interp.recR(IntPrimOp.values.map(op => interp.fldR(op.toString.toLowerCase(), interp.clsR(interp.emptyEnv, "k5", "x",
            interp.letV("f6", interp.funV("j7", "y", interp.letV("k8", interp.prmV(interp.intP(op), Vector("x", "y")), interp.appC("j7", "k8"))),
                interp.appC("k5", "f6"))))).toVector)
    }
}
