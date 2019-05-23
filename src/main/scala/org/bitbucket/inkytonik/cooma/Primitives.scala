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

abstract class Primitive {
    def numArgs : Int

    def eval(interp : Interpreter, rho : Env, xs : Seq[String], args : Seq[String]) : ValueR =
        if (xs.length == numArgs)
            run(interp, rho, xs, args)
        else
            sys.error(s"$show: expected $numArgs arg(s), got $xs")

    def run(interp : Interpreter, rho : Env, xs : Seq[String], args : Seq[String]) : ValueR

    def show : String
}

case class ArgumentP(i : Int) extends Primitive {
    val numArgs = 0

    def run(interp : Interpreter, rho : Env, xs : Seq[String], args : Seq[String]) : ValueR =
        if ((i < 0) || (i >= args.length))
            ErrR(s"command-line argument $i does not exist (arg count = ${args.length})")
        else
            StrR(args(i))

    def show = s"arg $i"
}

case class CapabilityP(cap : String) extends Primitive {
    import org.bitbucket.inkytonik.cooma.Capabilities.capability

    val numArgs = 1

    def run(interp : Interpreter, rho : Env, xs : Seq[String], args : Seq[String]) : ValueR =
        capability(interp, rho, cap, xs(0))

    def show = s"cap $cap"
}

case class ConsoleWriteP(filename : String) extends Primitive {
    import java.nio.file.{Files, Paths}

    val numArgs = 1

    def run(interp : Interpreter, rho : Env, xs : Seq[String], args : Seq[String]) : ValueR = {
        val x = xs(0)
        val s = interp.lookupR(rho, x) match {
            case IntR(i) =>
                i.toString
            case StrR(s) =>
                s
            case v =>
                sys.error(s"$show: can't write $v")
        }
        Files.write(Paths.get(filename), s.getBytes())
        RowR(Vector())
    }

    def show = s"consoleWrite $filename"
}

case class ReaderReadP(filename : String) extends Primitive {
    import org.bitbucket.inkytonik.kiama.util.FileSource

    val numArgs = 1

    def run(interp : Interpreter, rho : Env, xs : Seq[String], args : Seq[String]) : ValueR =
        StrR(FileSource(filename).content)

    def show = s"readerRead $filename"
}

case class RowConcatP() extends Primitive {
    val numArgs = 2

    def run(interp : Interpreter, rho : Env, xs : Seq[String], args : Seq[String]) : ValueR = {
        val Vector(l, r) = xs
        interp.lookupR(rho, l) match {
            case RowR(lFields) =>
                interp.lookupR(rho, r) match {
                    case RowR(rFields) =>
                        RowR(lFields ++ rFields)
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

    def run(interp : Interpreter, rho : Env, xs : Seq[String], args : Seq[String]) : ValueR = {
        val Vector(r, f1) = xs
        interp.lookupR(rho, r) match {
            case RowR(fields) =>
                fields.collectFirst {
                    case FldR(f2, v) if f1 == f2 =>
                        v
                } match {
                    case Some(v) =>
                        v
                    case None =>
                        sys.error(s"$show: can't find field $f1 in $fields")
                }

            case err : ErrR =>
                err

            case v =>
                sys.error(s"$show: $r is $v, looking for field $f1")
        }
    }

    def show = "select"
}
