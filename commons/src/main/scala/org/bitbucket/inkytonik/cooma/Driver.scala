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
import org.bitbucket.inkytonik.kiama.util.CompilerBase

abstract class Driver extends CompilerBase[ASTNode, Program, Config] {

    import org.bitbucket.inkytonik.cooma.CoomaParserPrettyPrinter.{any, layout}
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

    override def format(prog : Program) : Document =
        CoomaParserPrettyPrinter.format(prog, 5)

    def createREPL(config : Config) : REPL with Compiler with Backend
}
