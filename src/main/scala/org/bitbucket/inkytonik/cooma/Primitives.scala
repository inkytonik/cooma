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

abstract class Primitive
case class ConsoleWrite(filename : String) extends Primitive
case class ReaderRead(filename : String) extends Primitive

object Primitives {

    import java.nio.file.{Files, Paths}
    import org.bitbucket.inkytonik.kiama.util.FileSource

    val unit = RowR(Vector())

    def primitive(interpreter : Interpreter, rho : Env, primitive : Primitive, args : Seq[String]) : ValueR =
        (primitive, args) match {
            case (ConsoleWrite(f), Vector(x)) =>
                val s = interpreter.lookupR(rho, x) match {
                    case IntR(i) =>
                        i.toString
                    case StrR(s) =>
                        s
                    case v =>
                        sys.error(s"interpretValue: can't write $v")
                }
                Files.write(Paths.get(f), s.getBytes())
                unit

            case (ReaderRead(f), Vector(x)) =>
                StrR(FileSource(f).content)

            case (name, arg) =>
                sys.error(s"interpretValue: unknown primitive call $name $arg")
        }

}
