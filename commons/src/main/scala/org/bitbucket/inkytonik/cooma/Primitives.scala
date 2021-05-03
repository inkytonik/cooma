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

import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._

object Primitives {

    def allInt1PrimBinOps : Vector[UserPrimitive] =
        Vector(
            IntAbsP()
        )

    def allInt2PrimBinOps =
        Vector(
            IntAddP(),
            IntDivP(),
            IntMulP(),
            IntPowP(),
            IntSubP()
        )

    def allIntPrimRelOps =
        Vector(
            IntGteP(),
            IntGtP(),
            IntLteP(),
            IntLtP()
        )

    def primName(prim : UserPrimitive) : String =
        prim.productPrefix.dropRight(1)

    def primFunName(prim : UserPrimitive) : String =
        primName(prim).drop(3).toLowerCase()

    case class CoomaException(
        exceptionType : CoomaExceptionType.Value,
        prefix : String,
        message : String
    ) extends Throwable

    object CoomaExceptionType extends Enumeration {
        val Cap = Value("CapabilityException")
        val Prim = Value("PrimitiveException")
    }

    def errCap(capability: String, message: String): Nothing =
        throw CoomaException(CoomaExceptionType.Cap, capability, message)

    def errPrim(primitive: String, message: String): Nothing =
        throw CoomaException(CoomaExceptionType.Prim, primitive, message)

}

trait Primitives {

    self : Backend =>

    import java.io._
    import java.nio.file.Paths

    import org.bitbucket.inkytonik.cooma.PrettyPrinter.show
    import org.bitbucket.inkytonik.cooma.PrimitiveUtils.readReaderContents
    import org.bitbucket.inkytonik.cooma.Primitives.{errCap, errPrim}
    import org.bitbucket.inkytonik.cooma.Util.{escape, fresh, unescape}
    import scalaj.http.Http

    def evalPrim(p : Primitive, rho : Env,
        xs : Seq[String], args : Seq[String]) : ValueR = {
        val nArgs = numArgs(p)
        if (xs.length == nArgs)
            run(p, rho, xs, args)
        else
            errPrim(show(p), s"expected $nArgs arg(s), got $xs")
    }

    def numArgs(p : Primitive) : Int =
        p match {
            case ArgumentP(_) =>
                0
            case CapabilityP(_) | FolderReaderReadP(_) | HttpClientP(_, _) |
                ReaderReadP(_) | WriterWriteP(_) =>
                1
            case RecConcatP() | RecSelectP() | FolderWriterWriteP(_) =>
                2
            case UserP(u) =>
                u match {
                    case IntAbsP() | StrLengthP() =>
                        1
                    case IntAddP() | IntDivP() | IntGtP() | IntGteP() | IntLtP() |
                        IntLteP() | IntMulP() | IntPowP() | IntSubP() |
                        StrConcatP() | StrSubstrP() | VecLengthP() =>
                        2
                    case EqualP() | VecConcatP() | VecGetP() | VecAppendP() |
                        VecPrependP() =>
                        3
                    case VecPutP() =>
                        4
                }
        }

    def getStrParam(show : String, rho : Env, x : String) : String =
        isStrR(lookupR(rho, x)) match {
            case Some(v) =>
                v
            case _ =>
                errPrim(show, s"can't find string operand $x")
        }

    def getIntParam(show : String, rho : Env, x : String) : BigInt =
        isIntR(lookupR(rho, x)) match {
            case Some(v) =>
                v
            case _ =>
                errPrim(show, s"can't find integer operand $x")
        }

    def run(p : Primitive, rho : Env, xs : Seq[String], args : Seq[String]) : ValueR =
        p match {
            case ArgumentP(i) =>
                argument(i, args)

            case CapabilityP(cap) =>
                capability(cap, rho, xs(0))

            case FolderReaderReadP(filename) =>
                folderReaderRead(rho, filename, xs(0))

            case FolderWriterWriteP(filename) =>
                folderWriterWrite(rho, filename, xs(0), xs(1))

            case HttpClientP(method, url) =>
                httpClient(rho, method, url, xs(0))

            case ReaderReadP(filename) =>
                readerRead(filename)

            case RecConcatP() =>
                recConcat(rho, xs(0), xs(1))

            case RecSelectP() =>
                recSelect(rho, xs(0), xs(1))

            case WriterWriteP(filename) =>
                writerWrite(filename, rho, xs(0))

            case UserP(u) =>
                u match {
                    case EqualP() =>
                        equal(rho, xs(1), xs(2))
                    case IntAbsP() =>
                        intUnPrim("intAbs", rho, xs(0), _.abs)
                    case IntAddP() =>
                        intBinPrim("intAdd", rho, xs(0), xs(1), _ + _)
                    case IntDivP() =>
                        intDiv(rho, xs(0), xs(1))
                    case IntGtP() =>
                        intRelPrim("intGt", rho, xs(0), xs(1), _ > _)
                    case IntGteP() =>
                        intRelPrim("intGte", rho, xs(0), xs(1), _ >= _)
                    case IntLtP() =>
                        intRelPrim("intLt", rho, xs(0), xs(1), _ < _)
                    case IntLteP() =>
                        intRelPrim("intLte", rho, xs(0), xs(1), _ <= _)
                    case IntMulP() =>
                        intBinPrim("intMul", rho, xs(0), xs(1), _ * _)
                    case IntPowP() =>
                        intPow(rho, xs(0), xs(1))
                    case IntSubP() =>
                        intBinPrim("intSub", rho, xs(0), xs(1), _ - _)
                    case StrConcatP() =>
                        strConcat(rho, xs(0), xs(1))
                    case StrLengthP() =>
                        strLength(rho, xs(0))
                    case StrSubstrP() =>
                        strSubstr(rho, xs(0), xs(1))
                    case VecAppendP() =>
                        vecAppend(rho, xs(1), xs(2))
                    case VecConcatP() =>
                        vecConcat(rho, xs(1), xs(2))
                    case VecGetP() =>
                        vecGet(rho, xs(1), xs(2))
                    case VecLengthP() =>
                        vecLength(rho, xs(1))
                    case VecPrependP() =>
                        vecPrepend(rho, xs(1), xs(2))
                    case VecPutP() =>
                        vecPut(rho, xs(1), xs(2), xs(3))
                }
        }

    def argument(i : Int, args : Seq[String]) : ValueR =
        if ((i < 0) || (i >= args.length))
            errPrim("Argument", s"command-line argument $i does not exist (arg count = ${args.length})")
        else
            strR(args(i))

    def capability(cap : String, rho : Env, x : String) : ValueR = {

        def makeCapability(pairs : Vector[(String, Primitive, Int)]) : ValueR =
            recR(pairs.map {
                case (fieldName, primitive, numArgs) =>
                    val p = fresh("p")
                    def aux(numArgs : Int, args : Vector[String], k0 : String) : Term = {
                        val k = fresh("k")
                        val y = fresh("y")
                        if (numArgs > 0)
                            letV(
                                p,
                                funV(k, y, aux(numArgs - 1, args :+ y, k)),
                                appC(idnC(k0), p)
                            )
                        else
                            letV(p, prmV(primitive, args),
                                appC(idnC(k0), p))
                    }
                    val k = fresh("k")
                    val y = fresh("y")
                    fldR(
                        fieldName,
                        clsR(
                            k, y, emptyEnv,
                            aux(numArgs - 1, Vector(y), k)
                        )
                    )
            })

        val value = lookupR(rho, x)
        val argument = isStrR(value) match {
            case Some(s) => s
            case None    => errCap(cap, s"got non-String argument $value")
        }

        cap match {
            case "FolderReader" =>
                makeCapability(Vector(("read", FolderReaderReadP(argument), 1)))
            case "FolderWriter" =>
                makeCapability(Vector(("write", FolderWriterWriteP(argument), 2)))
            case "HttpDelete" | "HttpGet" | "HttpPost" | "HttpPut" =>
                val method = cap.drop(4).toLowerCase()
                makeCapability(Vector((method, HttpClientP(method, argument), 1)))
            case "Reader" =>
                makeCapability(Vector(("read", ReaderReadP(argument), 1)))
            case "Writer" =>
                makeCapability(Vector(("write", WriterWriteP(argument), 1)))
        }
    }

    def equal(rho : Env, l : String, r : String) : ValueR = {

        def getField(f : String, r : Vector[FldR]) : Option[FldR] =
            r.find(getFieldName(_) == f)

        def equalValues(lvalue : ValueR, rvalue : ValueR) : Boolean = {
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
                                                equalValues(getFieldValue(lfld), getFieldValue(rfld))
                                            case None =>
                                                errPrim("equal", s"can't find field $f in $rfs")
                                        }
                                    })
                                case _ =>
                                    (isVarR(lvalue), isVarR(rvalue)) match {
                                        case (Some((lc, lv)), Some((rc, rv))) =>
                                            (lc == rc) && equalValues(lv, rv)
                                        case _ =>
                                            (isVecR(lvalue), isVecR(rvalue)) match {
                                                case (Some(lv), Some(rv)) =>
                                                    lv.corresponds(rv)(equalValues)
                                                case _ =>
                                                    false
                                            }
                                    }
                            }
                    }
            }
        }

        if (equalValues(lookupR(rho, l), lookupR(rho, r)))
            trueR
        else
            falseR
    }

    def folderFile(primName : String, rho : Env, root : String, suffixIdn : String) : File = {
        val suffix = lookupR(rho, suffixIdn)
        val filename =
            isStrR(suffix)
                .map(suffix => s"$root/$suffix")
                .getOrElse(errCap(primName, s"expected String, got $suffix"))
        if (Paths.get(filename).normalize.startsWith(Paths.get(root).normalize)) new File(filename)
        else errCap(primName, s"$filename is not a descendant of $root")
    }

    def folderReaderRead(rho : Env, root : String, suffixIdn : String) : ValueR = {
        val file = folderFile("FolderReaderRead", rho, root, suffixIdn)
        val in = new BufferedReader(new FileReader(file))
        try {
            strR(readReaderContents(in))
        } catch {
            case e : IOException =>
                errCap("FolderReaderRead", e.getMessage)
        }
    }

    def folderWriterWrite(rho : Env, root : String, suffixIdn : String, x : String) : ValueR = {
        val file = folderFile("FolderWriterWrite", rho, root, suffixIdn)
        val text = {
            val text = lookupR(rho, x)
            isStrR(text).getOrElse(errCap("FolderWriterWrite", s"can't write $text"))
        }
        val out = new BufferedWriter(new FileWriter(file))
        try {
            out.write(text)
        } catch {
            case e : IOException =>
                errCap("FolderWriterWrite", e.getMessage)
        } finally {
            out.close()
        }
        recR(Vector())
    }

    def httpClient(rho : Env, methodName : String, url : String, x : String) : ValueR =
        isStrR(lookupR(rho, x)) match {
            case Some(suffix) =>
                val (code, body) = {
                    val response = Http(url + suffix).method(methodName).asString
                    (response.code, response.body)
                }
                recR(Vector(
                    fldR("code", intR(code)),
                    fldR("body", strR(body))
                ))
            case None =>
                errCap("HttpClient", s"can't find string operand $x")
        }

    def intBinPrim(show : String, rho : Env, l : String, r : String, op : (BigInt, BigInt) => BigInt) : ValueR = {
        val li = getIntParam(show, rho, l)
        val ri = getIntParam(show, rho, r)
        intR(op(li, ri))
    }

    def intDiv(rho : Env, l : String, r : String) : ValueR = {
        val ri = getIntParam("intDiv", rho, r)
        if (ri == 0)
            errPrim("IntDiv", s"division by zero")
        else {
            val li = getIntParam("intDiv", rho, l)
            intR(li / ri)
        }
    }

    def intPow(rho : Env, l : String, r : String) : ValueR = {
        val ri = getIntParam("intPow", rho, r)
        if (ri < 0)
            errPrim("IntPow", s"illegal negative power $ri given")
        else {
            val li = getIntParam("intPow", rho, l)
            intR(li.pow(ri.toInt))
        }
    }

    def intRelPrim(show : String, rho : Env, l : String, r : String, op : (BigInt, BigInt) => Boolean) : ValueR = {
        val li = getIntParam(show, rho, l)
        val ri = getIntParam(show, rho, r)
        if (op(li, ri)) trueR else falseR
    }

    def intUnPrim(show : String, rho : Env, i : String, op : BigInt => BigInt) : ValueR = {
        val ii = getIntParam(show, rho, i)
        intR(op(ii))
    }

    def readerRead(filename : String) : ValueR = {
        if (CoomaConstants.CONSOLEIO != filename && !PrimitiveUtils.isFileReadable(filename))
            errCap("ReaderRead", s"can't read $filename")
        lazy val in : BufferedReader =
            new BufferedReader(
                filename match {
                    case CoomaConstants.CONSOLEIO => new InputStreamReader(System.in)
                    case _                        => new BufferedReader(new FileReader(filename))
                }
            )
        try {
            val s = readReaderContents(in)
            strR(escape(s))
        } catch {
            case e : IOException =>
                errCap("ReaderRead", e.getMessage)
        }
    }

    def recConcat(rho : Env, l : String, r : String) : ValueR = {
        val vl = lookupR(rho, l)
        val vr = lookupR(rho, r)
        def aux(v : ValueR, side : String) : Vector[FldR] =
            isRecR(v) match {
                case Some(fields) =>
                    fields
                case None =>
                    errPrim("recConcat", s"$side argument $r of record concatenation is non-record")
            }
        recR(aux(vl, "first") ++ aux(vr, "second"))
    }

    def recSelect(rho : Env, r : String, f : String) : ValueR = {
        val value = lookupR(rho, r)
        isRecR(value) match {
            case Some(fields) =>
                fields.collectFirst {
                    case fld if isFldR(fld).isDefined && getFieldName(fld) == f =>
                        getFieldValue(fld)
                } match {
                    case Some(v) => v
                    case None =>
                        errPrim("recSelect", s"can't find field $f in $fields")
                }
            case None => errPrim("recSelect", s"$r is non-record $value, looking for field $f")
        }
    }

    def strConcat(rho : Env, x : String, y : String) : ValueR = {
        val sx = getStrParam("strConcat", rho, x)
        val sy = getStrParam("strConcat", rho, y)
        strR(escape(unescape(sx) + unescape(sy)))
    }

    def strLength(rho : Env, x : String) : ValueR = {
        val sx = getStrParam("strLength", rho, x)
        intR(unescape(sx).length)
    }

    def strSubstr(rho : Env, x : String, i : String) : ValueR = {
        val sx = getStrParam("strSubstr", rho, x)
        val usx = unescape(sx)
        val ii = getIntParam("strSubstr", rho, i)
        if ((ii < 0) || (ii > usx.length))
            errPrim("strSubstr", s"""index $ii out of range for string "$sx"""")
        else
            strR(escape(usx.substring(ii.toInt)))
    }

    def lookupVector(rho : Env, name : String) : Vector[ValueR] =
        isVecR(lookupR(rho, name)) match {
            case Some(value) =>
                value
            case None =>
                errPrim("lookupVector", s"$name is ${lookupR(rho, name)}, expected Vector value")
        }

    def vecAppend(rho : Env, v : String, x : String) : ValueR = {
        val elems = lookupVector(rho, v)
        vecR(elems :+ lookupR(rho, x))
    }

    def vecConcat(rho : Env, v : String, w : String) : ValueR =
        vecR(lookupVector(rho, v) ++ lookupVector(rho, w))

    def vecGet(rho : Env, v : String, i : String) : ValueR =
        isIntR(lookupR(rho, i)) match {
            case Some(i) =>
                val elems = lookupVector(rho, v)
                val index = i.toInt
                if (elems.indices contains index)
                    elems(index)
                else
                    errPrim("vecGet", s"vector index out of bounds - size: ${elems.size}, index: $index")
            case _ =>
                errPrim("vecGet", s"can't find integer (index) operand $i")
        }

    def vecLength(rho : Env, v : String) : ValueR =
        intR(lookupVector(rho, v).length)

    def vecPrepend(rho : Env, v : String, x : String) : ValueR = {
        val elems = lookupVector(rho, v)
        vecR(lookupR(rho, x) +: elems)
    }

    def vecPut(rho : Env, v : String, i : String, x : String) : ValueR = {
        val elems = lookupVector(rho, v)
        isIntR(lookupR(rho, i)) match {
            case Some(idx) =>
                if (elems.indices contains idx)
                    vecR(elems.updated(idx.intValue, lookupR(rho, x)))
                else
                    errPrim("vecPut", s"vector index out of bounds - size: ${elems.size}, index: $idx")
            case None =>
                errPrim("vecPut", s"can't find index operand $i")
        }
    }

    def writerWrite(filename : String, rho : Env, x : String) : ValueR = {
        if (CoomaConstants.CONSOLEIO != filename && !PrimitiveUtils.isFileWritable(filename))
            errCap("WriterWrite", s"can't write $filename")
        val value = lookupR(rho, x)
        val s = isIntR(value) match {
            case Some(i) =>
                i.toString
            case None =>
                isStrR(value) match {
                    case Some(s) =>
                        unescape(s)
                    case None =>
                        errPrim("writerWrite", s"can't write $value")
                }
        }
        val out : Writer =
            filename match {
                case CoomaConstants.CONSOLEIO =>
                    stdout
                case _ =>
                    new BufferedWriter(new FileWriter(filename))
            }
        try {
            out.write(s)
        } catch {
            case e : IOException =>
                errPrim("writerWrite", e.getMessage)
        } finally {
            out.close()
        }
        recR(Vector())
    }

}
