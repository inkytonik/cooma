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

abstract class Driver extends CompilerBase[ASTNode, Program, Config] with Server {

    import org.bitbucket.inkytonik.cooma.CoomaParserPrettyPrinter.{any, layout, pretty}
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document
    import org.bitbucket.inkytonik.kiama.relation.Tree
    import org.bitbucket.inkytonik.kiama.util.Messaging.{Messages, noMessages}
    import org.bitbucket.inkytonik.kiama.util.Source

    val name = "cooma"

    def createConfig(args : Seq[String]) : Config =
        new Config(args)

    override def driver(args : Seq[String]) : Unit = {
        createAndInitConfig(args) match {
            case Left(message) =>
                System.err.println(s"cooma: $message, use --help for options")
            case Right(config) =>
                run(config)
        }
    }

    def isREPL(config : Config) =
        config.filenames().isEmpty

    override def compileFiles(config : Config) : Unit = {
        if (!isREPL(config))
            compileFile(config.filenames()(0), config)
    }

    val analysers = scala.collection.mutable.Map[Source, SemanticAnalyser]()

    override def makeast(source : Source, config : Config) : Either[Program, Messages] = {
        val p = new CoomaParser(source, positions)
        val pr = p.pProgram(0)
        if (pr.hasValue) {
            val program = p.value(pr).asInstanceOf[Program]
            if (config.coomaASTPrint())
                config.output().emitln(layout(any(program), 5))
            if (config.server()) {
                publishSourceProduct(source, format(program))
                publishSourceTreeProduct(source, pretty(any(program)))
            }
            checkProgram(program, source, config) match {
                case Vector() =>
                    Left(program)
                case messages =>
                    Right(messages)
            }
        } else
            Right(Vector(p.errorToMessage(pr.parseError)))
    }

    def checkProgram(program : Program, source : Source, config : Config) : Messages =
        if (!config.server() && config.filenames().isEmpty) {
            noMessages
        } else {
            val tree = new Tree[ASTNode, Program](program)
            val analyser = new SemanticAnalyser(tree)
            analysers.get(source) match {
                case Some(prevAnalyser) =>
                    positions.resetAllAt(prevAnalyser.tree.nodes)
                case _ =>
                // Do nothing
            }
            analysers(source) = analyser
            if (config.typePrint())
                analyser.tipe(program.expression) match {
                    case Some(t) =>
                        config.output().emitln(show(t))
                    case None =>
                        config.output().emitln("unknown type")
                }
            analyser.errors
        }

    override def format(prog : Program) : Document =
        CoomaParserPrettyPrinter.format(prog, 5)

    def createREPL(config : Config) : REPL with Compiler with Backend

}
