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

    import org.bitbucket.inkytonik.cooma.CoomaParserPrettyPrinter.{any, layout, pretty, show}
    import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
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
            analyser.tipe(program.expression) match {
                case Some(tipe) =>
                    if (config.typePrint())
                        config.output().emitln(show(analyser.alias(tipe)))
                    if (analyser.errors.isEmpty && config.usage())
                        printUsage(program.expression, tipe, config)
                case None =>
                    if (config.typePrint())
                        config.output().emitln("unknown type")
            }
            analyser.errors
        }

    def printUsage(expression : Expression, tipe : Expression, config : Config) : Unit = {

        def printArgument(argument : Argument) : Unit = {
            config.output().emit(s"  ${argument.idnDef.identifier}: ")
            argument.expression match {
                case ReaderT() =>
                    config.output().emit("a reader")
                case ReaderWriterT() =>
                    config.output().emit("a reader writer")
                case StrT() =>
                    config.output().emit("a string")
                case WriterT() =>
                    config.output().emit("a writer")
                case tipe =>
                    config.output().emit(s"unsupported argument type ${show(tipe)}")
            }
            config.output().emitln("")
        }

        def printArguments(arguments : Arguments) : Unit =
            arguments.optArguments match {
                case Vector() =>
                    config.output().emitln("  none")
                case arguments =>
                    arguments.map(printArgument)
            }

        def printResultType(resultType : Expression) : Unit =
            config.output().emitln(s"result type:\n  ${show(resultType)}")

        config.output().emitln("arguments:")
        (expression, tipe) match {
            case (Fun(arguments, _), FunT(_, resultType)) =>
                printArguments(arguments)
                printResultType(resultType)
            case _ =>
                printArguments(Arguments(Vector()))
                printResultType(tipe)
        }

    }

    override def format(prog : Program) : Document =
        CoomaParserPrettyPrinter.format(prog, 5)

    def createREPL(config : Config) : REPL with Compiler with Backend

}
