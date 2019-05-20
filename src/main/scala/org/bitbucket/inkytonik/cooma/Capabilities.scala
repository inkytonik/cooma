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

object Capabilities {

    import java.nio.file.{Files, Paths}
    import org.bitbucket.inkytonik.cooma.Util.fresh

    def capability(interpreter : Interpreter, rho : Env, name : String, x : String) : ValueR = {

        val argument =
            interpreter.lookupR(rho, x) match {
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
                    makeCapability("write", ConsoleWrite(argument))
                else
                    ErrR(s"Console capability unavailable: can't write $argument")

            case "Reader" =>
                if (Files.isReadable(Paths.get(argument)))
                    makeCapability("read", ReaderRead(argument))
                else
                    ErrR(s"Reader capability unavailable: can't read $argument")

            case _ =>
                sys.error(s"capability: unknown primitive $name")
        }

    }

}
