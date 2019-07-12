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

import org.bitbucket.inkytonik.cooma.CoomaParserSyntax.{ASTNode, Program}
import org.bitbucket.inkytonik.cooma.graalvm.GraalVMBackend
import org.bitbucket.inkytonik.kiama.util.CompilerBase

class Driver extends CompilerBase[ASTNode, Program, Config] {

    import org.bitbucket.inkytonik.cooma.CoomaParserPrettyPrinter.{any, layout}
    import org.bitbucket.inkytonik.cooma.reference.ReferenceBackend
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document
    import org.bitbucket.inkytonik.kiama.util.Messaging.Messages
    import org.bitbucket.inkytonik.kiama.util.Source

    val name = "cooma"

    def createConfig(args : Seq[String]) : Config =
        new Config(args)

    override def driver(args : Seq[String]) {
        createAndInitConfig(args) match {
            case Left(message) =>
                System.err.println(s"cooma: $message, use --help to see list")
            case Right(config) =>
                run(config)
        }
    }

    def createREPL(config : Config) : REPL with Compiler with Backend =
        if (config.graalVM())
            new REPL with Compiler with GraalVMBackend
        else
            new REPL with Compiler with ReferenceBackend

    override def run(config : Config) {
        if (config.filenames().isEmpty) {
            val repl = createREPL(config)
            repl.driver(config.args)
        } else
            super.run(config)
    }

    override def compileFiles(config : Config) {
        val args = config.filenames()
        if (args.length >= 1)
            compileFile(args(0), config)
    }

    override def makeast(source : Source, config : Config) : Either[Program, Messages] = {
        val p = new CoomaParser(source, positions)
        val pr = p.pProgram(0)
        if (pr.hasValue) {
            val prog = p.value(pr).asInstanceOf[Program]
            if (config.coomaASTPrint())
                config.output().emitln(layout(any(prog), 5))
            Left(prog)
        } else
            Right(Vector(p.errorToMessage(pr.parseError)))
    }

    def process(source : Source, prog : Program, config : Config) {

        val system = if (config.graalVM()) new Compiler with GraalVMBackend else new Compiler with ReferenceBackend

        val term = system.compileCommand(prog)
        if (config.irPrint())
            config.output().emitln(system.showTerm(term))
        if (config.irASTPrint())
            config.output().emitln(layout(any(term), 5))

        val args = config.filenames().tail
        system.interpret(term, args, config)
    }

    override def format(prog : Program) : Document =
        CoomaParserPrettyPrinter.format(prog, 5)

}
