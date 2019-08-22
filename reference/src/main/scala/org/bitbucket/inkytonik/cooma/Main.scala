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
        if (config.graalVM()) new TruffleFrontend().interpret(config) else new ReferenceFrontend().interpret(config)
    }
}

class ReferenceFrontend extends Frontend {
    val driver = new ReferenceDriver()
    override def interpret(config : Config) : Unit = {
        driver.run(config)
    }

    /**
     * This method is mainly built for the tests that execute cooma strings instead of
     * cooma files.
     * @param programName
     * @param program
     * @param config
     */
    override def interpret(programName : String, program : String, config : Config) : Unit = {
        driver.compileString(programName, program, config)
    }

}

class ReferenceDriver extends Driver {

    import org.bitbucket.inkytonik.cooma.CoomaParserPrettyPrinter.{any, layout}

    override def run(config : Config) {
        if (config.filenames().isEmpty) {
            val repl = createREPL(config)
            repl.driver(config.args)
        } else
            super.run(config)
    }

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
