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

object Main {

    import org.bitbucket.inkytonik.cooma.truffle.TruffleFrontend
    import org.rogach.scallop.exceptions.UnknownOption

    def main(args : Array[String]) : Unit = {
        val config = new Config(args.toIndexedSeq)
        try {
            config.verify()
            if (config.graalVM())
                new TruffleFrontend().interpret(config)
            else
                new ReferenceFrontend().interpret(config)
        } catch {
            case e : UnknownOption =>
                println(s"cooma: ${e.getMessage()}, use --help for options")
        }
    }
}
