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

import java.net.SocketException

import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
import org.bitbucket.inkytonik.cooma.primitive.database.Metadata
import org.bitbucket.inkytonik.cooma.primitive.{Database, FileIo, HttpServer}

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

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

    def allStrPrimRelOps =
        Vector(
            StrGteP(),
            StrGtP(),
            StrLteP(),
            StrLtP()
        )

    def primName(prim : Product) : String =
        prim.productPrefix.dropRight(1)

    def primFunName(prim : UserPrimitive) : String =
        primName(prim).drop(3).toLowerCase()

}

trait Primitives extends Database with FileIo with HttpServer {

    self : Backend =>

    import java.io._
    import java.nio.file.Paths
    import scala.jdk.CollectionConverters._

    import org.bitbucket.inkytonik.cooma.CoomaException._
    import org.bitbucket.inkytonik.cooma.PrettyPrinter.show
    import org.bitbucket.inkytonik.cooma.PrimitiveUtils.readReaderContents
    import org.bitbucket.inkytonik.cooma.Primitives.primName
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
            case ArgumentP(_) | ArgumentCheckP(_) =>
                0
            case CapabilityP(_) | DbTableAllP(_, _) | DbTableDeleteP(_, _) | HttpServerP(_) |
                DbTableGetByIdP(_, _) | DbTableInsertP(_, _) | DbTableUpdateP(_, _) |
                FolderReaderReadP(_) | HttpClientP(_, _) | ReaderReadP(_) |
                RunnerRunP(_) | WriterWriteP(_) =>
                1
            case RecConcatP() | RecSelectP() | FolderRunnerRunP(_) | FolderWriterWriteP(_) =>
                2
            case UserP(u) =>
                u match {
                    case IntAbsP() | StrLengthP() =>
                        1
                    case IntAddP() | IntDivP() | IntGtP() | IntGteP() | IntLtP() |
                        IntLteP() | IntModP() | IntMulP() | IntPowP() | IntSubP() |
                        StrConcatP() | StrGteP() | StrGtP() | StrLteP() | StrLtP() |
                        StrSubstrP() | VecLengthP() =>
                        2
                    case EqualP() | VecConcatP() | VecGetP() | VecAppendP() |
                        VecPrependP() =>
                        3
                    case VecPutP() =>
                        4
                }
        }

    def getStrParam(prim : UserPrimitive, rho : Env, x : String) : String =
        isStrR(lookupR(rho, x)) match {
            case Some(v) =>
                v
            case _ =>
                errPrim(primName(prim), s"can't find string operand $x")
        }

    def getIntParam(prim : UserPrimitive, rho : Env, x : String) : BigInt =
        isIntR(lookupR(rho, x)) match {
            case Some(v) =>
                v
            case _ =>
                errPrim(primName(prim), s"can't find integer operand $x")
        }

    /**
     * @param prim  the `Primitive` object (as instantiated by `capability`)
     * @param rho   the environment
     * @param xs    primitive arguments
     * @param args  command-line arguments
     * @return      result (Cooma value) of primitive application
     */
    def run(prim : Primitive, rho : Env, xs : Seq[String], args : Seq[String]) : ValueR =
        prim match {
            case ArgumentP(i) =>
                argument(prim, i, args)

            case ArgumentCheckP(i) =>
                argumentCheck(i, args)

            case CapabilityP(cap) =>
                capability(cap, rho, xs(0))

            case DbTableAllP(index, tablename) =>
                dbAll(index, tablename)

            case DbTableDeleteP(index, tablename) =>
                dbDelete(index, tablename, lookupR(rho, xs(0)))

            case DbTableGetByIdP(index, tablename) =>
                dbGetById(index, tablename, lookupR(rho, xs(0)))

            case DbTableInsertP(index, tablename) =>
                dbInsert(index, tablename, lookupR(rho, xs(0)))

            case DbTableUpdateP(index, tablename) =>
                dbUpdate(index, tablename, lookupR(rho, xs(0)))

            case FolderReaderReadP(filename) =>
                folderReaderRead(prim, rho, filename, xs(0))

            case FolderRunnerRunP(filename) =>
                folderRunnerRun(prim, rho, filename, xs(0), xs(1))

            case FolderWriterWriteP(filename) =>
                folderWriterWrite(prim, rho, filename, xs(0), xs(1))

            case HttpClientP(method, url) =>
                httpClient(prim, rho, method, url, xs(0))

            case HttpServerP(port) =>
                httpServer(prim, rho, port, xs(0))

            case ReaderReadP(filename) =>
                readerRead(prim, filename)

            case RunnerRunP(filename) =>
                runnerRun(prim, filename, rho, xs(0))

            case RecConcatP() =>
                recConcat(prim, rho, xs(0), xs(1))

            case RecSelectP() =>
                recSelect(prim, rho, xs(0), xs(1))

            case WriterWriteP(filename) =>
                writerWrite(prim, filename, rho, xs(0))

            case UserP(prim) =>
                prim match {
                    case EqualP() =>
                        equal(prim, rho, xs(1), xs(2))
                    case IntAbsP() =>
                        intUnPrim(prim, rho, xs(0), _.abs)
                    case IntAddP() =>
                        intBinPrim(prim, rho, xs(0), xs(1), _ + _)
                    case IntDivP() =>
                        intDiv(prim, rho, xs(0), xs(1), _ / _)
                    case IntGtP() =>
                        intRelPrim(prim, rho, xs(0), xs(1), _ > _)
                    case IntGteP() =>
                        intRelPrim(prim, rho, xs(0), xs(1), _ >= _)
                    case IntLtP() =>
                        intRelPrim(prim, rho, xs(0), xs(1), _ < _)
                    case IntLteP() =>
                        intRelPrim(prim, rho, xs(0), xs(1), _ <= _)
                    case IntModP() =>
                        intDiv(prim, rho, xs(0), xs(1), _ % _)
                    case IntMulP() =>
                        intBinPrim(prim, rho, xs(0), xs(1), _ * _)
                    case IntPowP() =>
                        intPow(prim, rho, xs(0), xs(1))
                    case IntSubP() =>
                        intBinPrim(prim, rho, xs(0), xs(1), _ - _)
                    case StrConcatP() =>
                        strConcat(prim, rho, xs(0), xs(1))
                    case StrGtP() =>
                        strRelPrim(prim, rho, xs(0), xs(1), _ > _)
                    case StrGteP() =>
                        strRelPrim(prim, rho, xs(0), xs(1), _ >= _)
                    case StrLengthP() =>
                        strLength(prim, rho, xs(0))
                    case StrLtP() =>
                        strRelPrim(prim, rho, xs(0), xs(1), _ < _)
                    case StrLteP() =>
                        strRelPrim(prim, rho, xs(0), xs(1), _ <= _)
                    case StrSubstrP() =>
                        strSubstr(prim, rho, xs(0), xs(1))
                    case VecAppendP() =>
                        vecAppend(prim, rho, xs(1), xs(2))
                    case VecConcatP() =>
                        vecConcat(prim, rho, xs(1), xs(2))
                    case VecGetP() =>
                        vecGet(prim, rho, xs(1), xs(2))
                    case VecLengthP() =>
                        vecLength(prim, rho, xs(1))
                    case VecPrependP() =>
                        vecPrepend(prim, rho, xs(1), xs(2))
                    case VecPutP() =>
                        vecPut(prim, rho, xs(1), xs(2), xs(3))
                }
        }

    def argument(prim : Primitive, i : Int, args : Seq[String]) : ValueR =
        if ((i < 0) || (i >= args.length))
            errPrim("Argument", s"command-line argument $i does not exist (arg count = ${args.length})")
        else
            strR(args(i))

    def argumentCheck(i : Int, args : Seq[String]) : ValueR =
        if (args.length != i)
            errPrim("ArgumentCheck", s"expected $i argument(s), found ${args.length}")
        else
            uniR

    /**
     * @param cap       the capability specifier
     * @param rho       the environment
     * @param x         the command-line argument to be passed to the capability constructor
     * @return          the capability object
     */
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

        val DatabaseClientRegex = """DatabaseClient:([0-9]+):([a-zA-Z0-9+/=]+)""".r
        cap match {
            case DatabaseClientRegex(indexStr, spec) =>
                val metadata = Metadata.fromSpec(spec)
                val index = indexStr.toInt
                dbConfigure(argument, metadata, index.toInt)
                val tableCaps =
                    metadata.tables.map {
                        case Metadata.Table(tablename, _) =>
                            def mk(methodName : String, f : (Int, String) => Primitive) =
                                (methodName, f(index, tablename), 1)
                            val rec =
                                makeCapability(Vector(
                                    mk("all", DbTableAllP),
                                    mk("delete", DbTableDeleteP),
                                    mk("getById", DbTableGetByIdP),
                                    mk("insert", DbTableInsertP),
                                    mk("update", DbTableUpdateP)
                                ))
                            fldR(tablename, rec)
                    }
                recR(tableCaps.toVector)
            case "FolderReader" =>
                checkFolderReader(argument)
                makeCapability(Vector(("read", FolderReaderReadP(argument), 1)))
            case "FolderRunner" =>
                checkFolderRunner(argument)
                makeCapability(Vector(("run", FolderRunnerRunP(argument), 2)))
            case "FolderWriter" =>
                checkFolderWriter(argument)
                makeCapability(Vector(("write", FolderWriterWriteP(argument), 2)))
            case "HttpDelete" | "HttpGet" | "HttpPost" | "HttpPut" =>
                val method = cap.drop(4).toLowerCase()
                makeCapability(Vector((method, HttpClientP(method, argument), 1)))
            case "HttpServer" =>
                makeCapability(Vector(("start", HttpServerP(argument.toInt), 1)))
            case "Reader" =>
                checkReader(argument)
                makeCapability(Vector(("read", ReaderReadP(argument), 1)))
            case "Runner" =>
                checkRunner(argument)
                makeCapability(Vector(("run", RunnerRunP(argument), 1)))
            case "Writer" =>
                checkWriter(argument)
                makeCapability(Vector(("write", WriterWriteP(argument), 1)))
            case x =>
                errPrim("Capability", s"unknown capability $cap")
        }
    }

    def equal(prim : UserPrimitive, rho : Env, l : String, r : String) : ValueR = {

        def sameFields(l : Vector[FldR], r : Vector[FldR]) : Boolean =
            l.map(getFieldName).toSet == r.map(getFieldName).toSet

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
                                    if (sameFields(lfs, rfs))
                                        lfs.forall(lfld => {
                                            val f = getFieldName(lfld)
                                            getField(f, rfs) match {
                                                case Some(rfld) =>
                                                    equalValues(getFieldValue(lfld), getFieldValue(rfld))
                                                case None =>
                                                    errPrim(primName(prim), s"can't find field $f in $rfs")
                                            }
                                        })
                                    else
                                        false
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

    def folderFile(prim : Primitive, rho : Env, root : String, suffixIdn : String) : File = {
        val suffix = lookupR(rho, suffixIdn)
        val filename =
            isStrR(suffix)
                .map(suffix => s"$root/$suffix")
                .getOrElse(errCap(primName(prim), s"expected String, got $suffix"))
        if (Paths.get(filename).normalize.startsWith(Paths.get(root).normalize)) new File(filename)
        else errCap(primName(prim), s"$filename is not a descendant of $root")
    }

    def folderReaderRead(prim : Primitive, rho : Env, root : String, suffixIdn : String) : ValueR = {
        val file = folderFile(prim, rho, root, suffixIdn)
        val in = new BufferedReader(new FileReader(file))
        Try(readReaderContents(in)) match {
            case Success(value)           => varR("Right", strR(value))
            case Failure(e : IOException) => varR("Left", strR(e.toString))
            case Failure(e)               => throw e
        }
    }

    def folderRunnerRun(prim : Primitive, rho : Env, root : String, suffixIdn : String, x : String) : ValueR = {
        val filename = folderFile(prim, rho, root, suffixIdn).getPath
        Try(runnerRun(prim, filename, rho, x)) match {
            case Success(rr) =>
                varR("Right", rr)
            case Failure(e : IOException) =>
                varR("Left", strR(e.toString))
            case Failure(e) =>
                throw e
        }
    }

    def folderWriterWrite(prim : Primitive, rho : Env, root : String, suffixIdn : String, x : String) : ValueR = {
        val file = folderFile(prim, rho, root, suffixIdn)
        val text = {
            val text = lookupR(rho, x)
            isStrR(text).getOrElse(errCap(primName(prim), s"can't write $text"))
        }
        val out = new BufferedWriter(new FileWriter(file))
        Try(out.write(text)) match {
            case Success(()) =>
                out.close()
                varR("Right", uniR)
            case Failure(e : IOException) =>
                varR("Left", strR(e.toString))
            case Failure(e) =>
                throw e
        }
    }

    def httpClient(prim : Primitive, rho : Env, methodName : String, url : String, x : String) : ValueR =
        isStrR(lookupR(rho, x)) match {
            case Some(suffix) =>
                Try(Http(url + suffix).method(methodName).asString) match {
                    case Success(response) =>
                        varR("Right", recR(Vector(
                            fldR("code", intR(response.code)),
                            fldR("body", strR(response.body))
                        )))
                    case Failure(e @ (_ : IOException | _ : SocketException)) =>
                        varR("Left", strR(e.toString))
                    case Failure(e) =>
                        throw e
                }
            case None =>
                errCap(primName(prim), s"can't find string operand $x")
        }

    def httpServer(prim : Primitive, rho : Env, port : Int, endpointsIdn : String) : ValueR =
        isRecR(lookupR(rho, endpointsIdn)) match {
            case Some(endpointsRec) =>
                val endpoints =
                    endpointsRec
                        .map { fld =>
                            val name = getFieldName(fld)
                            val endpoint = getFieldValue(fld)
                            name -> endpoint
                        }
                        .toMap
                serverStart(port, rho, endpoints)
            case None =>
                errCap(primName(prim), "expected record")
        }

    def intBinPrim(prim : UserPrimitive, rho : Env, l : String, r : String, op : (BigInt, BigInt) => BigInt) : ValueR = {
        val li = getIntParam(prim, rho, l)
        val ri = getIntParam(prim, rho, r)
        intR(op(li, ri))
    }

    def intDiv(prim : UserPrimitive, rho : Env, l : String, r : String, func : (BigInt, BigInt) => BigInt) : ValueR = {
        val ri = getIntParam(prim, rho, r)
        if (ri == 0)
            errPrim(primName(prim), s"division by zero")
        else {
            val li = getIntParam(prim, rho, l)
            intR(func(li, ri))
        }
    }

    def intPow(prim : UserPrimitive, rho : Env, l : String, r : String) : ValueR = {
        val ri = getIntParam(prim, rho, r)
        if (ri < 0)
            errPrim(primName(prim), s"illegal negative power $ri given")
        else {
            val li = getIntParam(prim, rho, l)
            intR(li.pow(ri.toInt))
        }
    }

    def intRelPrim(prim : UserPrimitive, rho : Env, l : String, r : String, op : (BigInt, BigInt) => Boolean) : ValueR = {
        val li = getIntParam(prim, rho, l)
        val ri = getIntParam(prim, rho, r)
        if (op(li, ri)) trueR else falseR
    }

    def intUnPrim(prim : UserPrimitive, rho : Env, i : String, op : BigInt => BigInt) : ValueR = {
        val ii = getIntParam(prim, rho, i)
        intR(op(ii))
    }

    def readerRead(prim : Primitive, filename : String) : ValueR = {
        lazy val in : Try[BufferedReader] =
            Try(new BufferedReader(
                filename match {
                    case CoomaConstants.CONSOLEIO => new InputStreamReader(System.in)
                    case _                        => new BufferedReader(new FileReader(filename))
                }
            ))
        in.flatMap { in =>
            val result = Try(readReaderContents(in))
            if (result.isSuccess) in.close()
            result
        } match {
            case Success(s) =>
                varR("Right", strR(escape(s)))
            case Failure(e : IOException) =>
                varR("Left", strR(e.toString))
            case Failure(e) =>
                throw e
        }
    }

    def recConcat(prim : Primitive, rho : Env, l : String, r : String) : ValueR = {
        val vl = lookupR(rho, l)
        val vr = lookupR(rho, r)
        def aux(v : ValueR, side : String) : Vector[FldR] =
            isRecR(v) match {
                case Some(fields) =>
                    fields
                case None =>
                    errPrim(primName(prim), s"$side argument $r of record concatenation is non-record")
            }
        recR(aux(vl, "first") ++ aux(vr, "second"))
    }

    def recSelect(prim : Primitive, rho : Env, r : String, f : String) : ValueR = {
        val value = lookupR(rho, r)
        isRecR(value) match {
            case Some(fields) =>
                fields.collectFirst {
                    case fld if isFldR(fld).isDefined && getFieldName(fld) == f =>
                        getFieldValue(fld)
                } match {
                    case Some(v) => v
                    case None =>
                        errPrim(primName(prim), s"can't find field $f in $fields")
                }
            case None => errPrim(primName(prim), s"$r is non-record $value, looking for field $f")
        }
    }

    def runnerRun(prim : Primitive, filename : String, rho : Env, x : String) : ValueR = {
        val value = lookupR(rho, x)
        val cmdargs : Seq[String] =
            isVecR(value) match {
                case Some(vec) =>
                    vec.map(isStrR).foldLeft(Seq.empty[String]) {
                        case (out, Some(s)) => out :+ s
                        case (_, None)      => errPrim(primName(prim), "non-String argument")
                    }
                case None =>
                    errPrim(primName(prim), "non-Vec argument list")
            }
        val proc = new ProcessBuilder((filename +: cmdargs).asJava).start()
        proc.waitFor()
        val exitValue = proc.exitValue()
        val output = {
            val reader = new BufferedReader(new InputStreamReader(proc.getInputStream))
            val builder = new StringBuilder
            @tailrec
            def aux() : Unit = {
                val line = reader.readLine()
                if (line != null) {
                    builder.append(s"$line\n")
                    aux()
                }
            }
            aux()
            reader.close()
            builder.result()
        }
        recR(Vector(
            fldR("exitValue", intR(exitValue)),
            fldR("output", strR(escape(output)))
        ))
    }

    def strConcat(prim : UserPrimitive, rho : Env, x : String, y : String) : ValueR = {
        val sx = getStrParam(prim, rho, x)
        val sy = getStrParam(prim, rho, y)
        strR(escape(unescape(sx) + unescape(sy)))
    }

    def strLength(prim : UserPrimitive, rho : Env, x : String) : ValueR = {
        val sx = getStrParam(prim, rho, x)
        intR(unescape(sx).length)
    }

    def strRelPrim(prim : UserPrimitive, rho : Env, l : String, r : String, op : (String, String) => Boolean) : ValueR = {
        val li = getStrParam(prim, rho, l)
        val ri = getStrParam(prim, rho, r)
        if (op(li, ri)) trueR else falseR
    }

    def strSubstr(prim : UserPrimitive, rho : Env, x : String, i : String) : ValueR = {
        val sx = getStrParam(prim, rho, x)
        val usx = unescape(sx)
        val ii = getIntParam(prim, rho, i)
        if ((ii < 0) || (ii > usx.length))
            errPrim(primName(prim), s"""index $ii out of range for string "$sx"""")
        else
            strR(escape(usx.substring(ii.toInt)))
    }

    def lookupVector(rho : Env, name : String) : Vector[ValueR] =
        isVecR(lookupR(rho, name)) match {
            case Some(value) =>
                value
            case None =>
                errPrim("LookupVector", s"$name is ${lookupR(rho, name)}, expected Vector value")
        }

    def vecAppend(prim : UserPrimitive, rho : Env, v : String, x : String) : ValueR = {
        val elems = lookupVector(rho, v)
        vecR(elems :+ lookupR(rho, x))
    }

    def vecConcat(prim : UserPrimitive, rho : Env, v : String, w : String) : ValueR =
        vecR(lookupVector(rho, v) ++ lookupVector(rho, w))

    def vecGet(prim : UserPrimitive, rho : Env, v : String, i : String) : ValueR =
        isIntR(lookupR(rho, i)) match {
            case Some(i) =>
                val elems = lookupVector(rho, v)
                val index = i.toInt
                if (elems.indices contains index)
                    elems(index)
                else
                    errPrim("VecGet", s"vector index out of bounds - size: ${elems.size}, index: $index")
            case _ =>
                errPrim(primName(prim), s"can't find integer (index) operand $i")
        }

    def vecLength(prim : UserPrimitive, rho : Env, v : String) : ValueR =
        intR(lookupVector(rho, v).length)

    def vecPrepend(prim : UserPrimitive, rho : Env, v : String, x : String) : ValueR = {
        val elems = lookupVector(rho, v)
        vecR(lookupR(rho, x) +: elems)
    }

    def vecPut(prim : UserPrimitive, rho : Env, v : String, i : String, x : String) : ValueR = {
        val elems = lookupVector(rho, v)
        isIntR(lookupR(rho, i)) match {
            case Some(idx) =>
                if (0 <= idx && idx < elems.length)
                    vecR(elems.updated(idx.intValue, lookupR(rho, x)))
                else
                    errPrim(primName(prim), s"vector index out of bounds - size: ${elems.size}, index: $idx")
            case None =>
                errPrim(primName(prim), s"can't find index operand $i")
        }
    }

    def writerWrite(prim : Primitive, filename : String, rho : Env, x : String) : ValueR = {
        val value = lookupR(rho, x)
        val s = isIntR(value) match {
            case Some(i) =>
                i.toString
            case None =>
                isStrR(value) match {
                    case Some(s) =>
                        unescape(s)
                    case None =>
                        errPrim(primName(prim), s"can't write $value")
                }
        }
        val out : Try[Writer] =
            filename match {
                case CoomaConstants.CONSOLEIO => Success(stdout)
                case _                        => Try(new BufferedWriter(new FileWriter(filename)))
            }
        out.flatMap { out =>
            val result = Try(out.write(s))
            if (result.isSuccess) out.close()
            result
        } match {
            case Success(())              => varR("Right", uniR)
            case Failure(e : IOException) => varR("Left", strR(e.toString))
            case Failure(e)               => throw e
        }
    }

}
