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

import org.bitbucket.inkytonik.cooma.backend.ReferenceBackend
import org.bitbucket.inkytonik.cooma.truffle.TruffleFrontend
import org.bitbucket.inkytonik.kiama.util.Source

object Main {
    def main(args : Array[String]) {
        val config = new Config(args)
        config.verify()
        val frontend = if (config.graalVM()) new TruffleFrontend else new ReferenceFrontend
        frontend.interpret(config)
    }

}

class ReferenceFrontend extends Frontend {
    override def interpret(config : Config) : Unit = {
        new ReferenceDriver(config)
    }
}

class ReferenceDriver(config : Config) extends Driver {

    import org.bitbucket.inkytonik.cooma.CoomaParserPrettyPrinter.{any, layout}

    super.driver(config.args)

    override def createREPL(config : Config) : REPL with Compiler with Backend = {
        new ReferenceBackend(config) with REPL with Compiler
    }

    /**
     *
     * @param source The original cooma Source
     * @param prog   The cooma source AST.
     * @param config
     */
    override def process(source : Source, prog : CoomaParserSyntax.Program, config : Config) : Unit = {
        val system = new ReferenceBackend(config) with Compiler
        val term = system.compileCommand(prog)
        if (config.irPrint())
            config.output().emitln(system.showTerm(term))
        if (config.irASTPrint())
            config.output().emitln(layout(any(term), 5))
        val args = config.filenames().tail
        system.interpret(term, args, config)
    }
}
