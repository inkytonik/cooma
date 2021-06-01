/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.cooma

import org.bitbucket.inkytonik.kiama.util.{REPLBase, StringSource}

trait REPL extends REPLBase[Config] {

    self : Compiler with Backend =>

    import org.bitbucket.inkytonik.cooma.PrettyPrinter.{any, layout, show}
    import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
    import org.bitbucket.inkytonik.cooma.SymbolTable._
    import org.bitbucket.inkytonik.kiama.relation.Tree
    import org.bitbucket.inkytonik.kiama.util.Messaging.Messages
    import org.bitbucket.inkytonik.kiama.util.{Console, Source, StringConsole}

    import scala.collection.mutable.ListBuffer

    override val prompt = "\ncooma> "

    val banner = s"""Cooma ${BuildInfo.version} REPL - ${this.backendName} backend
                    |
                    |Enter definitions or expressions (:help for commands)""".stripMargin

    /**
     * Counter of expression results.
     */
    var nResults = 0

    /**
     * Compile-time environment that keeps track of previously defined entities.
     */
    var currentStaticEnv : Environment = _

    /**
     * Create a configuration for this REPL session.
     */
    def createConfig(args : Seq[String]) : Config =
        new Config(args)

    /**
     * Start the REPL
     */
    override def driver(args : Seq[String]) : Unit = {
        super.driver(args)
    }

    def help(config : Config) : Unit = {
        config.output().emit(
            """
			  |exp                        evaluate exp, print value
			  |val x = exp                add new value definition
			  |def f(x : Int) Int = exp   add new function definition
			  |:help                      print this message
			  |:lines                     enter multiple separate input lines until :end or EOF
			  |:paste                     enter single multi-line input until :end or EOF
			  |:quit                      quit the REPL (also EOF)
			  |""".stripMargin
        )
    }

    def getLines(console : Console) : String = {
        val buf = ListBuffer[String]()
        var line = console.readLine("")
        while (line != null && line.trim != ":end") {
            buf.append(line + "\n")
            line = console.readLine("")
        }
        buf.mkString
    }

    def initialise(config : Config) : Unit = {
        currentStaticEnv = preludeStaticEnv(config)
        nResults = 0
    }

    override def processlines(config : Config) : Unit = {
        initialise(config)
        super.processlines(config)
    }

    /**
     * Process a line of user input.
     */
    override def processline(source : Source, console : Console, config : Config) : Option[Config] = {
        source.content match {
            case Command(Seq(":help")) =>
                help(config)
            case Command(Seq(":lines")) =>
                processconsole(new StringConsole(getLines(console)), "", config)
            case Command(Seq(":paste")) =>
                enterline(getLines(console), config)
            case Command(Seq(":quit")) =>
                return None
            case line =>
                enterline(line, config)
        }
        Some(config)
    }

    def enterline(line : String, config : Config) : Unit = {
        val source = new StringSource(line)
        val p = new CoomaParser(source, positions)
        val pr = p.pWhitespace(0)
        if (!pr.hasValue) {
            val pr = p.pREPLInput(0)
            if (pr.hasValue) {
                val input = p.value(pr).asInstanceOf[REPLInput]
                checkInput(input, config) match {
                    case Left(messages) =>
                        messaging.report(source, messages, config.output())
                    case Right((input, optTypeValue, optAliasedType)) =>
                        processInput(input, optTypeValue, optAliasedType, config)
                }
            } else
                config.output().emitln(p.formatParseError(pr.parseError, false))
        }
    }

    def defineLet(i : String, optT : Option[Expression], e : Expression) : REPLLet = {
        val v = Let(Val(), IdnDef(i), optT.map(LetType), e)
        currentStaticEnv = define(enter(currentStaticEnv), i, LetEntity(v))
        REPLLet(v)
    }

    def checkInput(
        input : REPLInput,
        config : Config
    ) : Either[Messages, (REPLInput, Option[Expression], Option[Expression])] = {
        if (config.coomaASTPrint())
            config.output().emitln(layout(any(input), 5))
        val tree = new Tree[ASTNode, REPLInput](input)
        val analyser = new SemanticAnalyser(tree, enter(currentStaticEnv))
        (analyser.errors, analyser.replTypeValue(input),
            analyser.replType(input), analyser.aliasedReplType(input)) match {
                case (Vector(), optTypeValue, replType, optAliasedReplType) =>
                    val input2 =
                        input match {
                            case REPLDef(Def(IdnDef(i), Body(as, t, e))) =>
                                defineLet(i, replType, Fun(as, e))

                            case REPLExp(Idn(_)) =>
                                input

                            case REPLExp(e) =>
                                val i = s"res$nResults"
                                nResults = nResults + 1
                                defineLet(i, replType, e)

                            case REPLLet(Let(_, IdnDef(i), None, e)) =>
                                defineLet(i, replType, e)

                            case REPLLet(Let(_, IdnDef(i), Some(LetType(t)), e)) =>
                                defineLet(i, analyser.unalias(e, t), e)
                        }
                    Right((input2, optTypeValue, optAliasedReplType))

                // case (Vector(), optTypeValue, replType, None) =>
                //     // sys.error(s"checkInput: couldn't find aliased REPL type for $input")
                //     Right((input, optTypeValue, ))

                case (messages, _, _, _) =>
                    Left(messages)
            }
    }

    /**
     * Embed an input entry for the REPL.
     */
    def processInput(input : REPLInput, optTypeValue : Option[Expression], optAliasedType : Option[Expression], config : Config) =
        input match {
            case REPLDef(fd @ Def(IdnDef(i), _)) =>
                processDef(i, fd, optTypeValue, optAliasedType, config)
            case REPLExp(e : Idn) =>
                processIdn(show(input), e, optTypeValue, optAliasedType, config)
            case REPLLet(vd @ Let(_, IdnDef(i), _, e)) =>
                processLet(i, vd, optTypeValue, optAliasedType, config)
            case _ =>
                sys.error(s"$input not supported for the moment")
        }

    /**
     * Process a user-entered value binding.
     */
    def processLet(i : String, ld : Let, optTypeValue : Option[Expression], optAliasedType : Option[Expression], config : Config) : Unit = {
        val program = Program(Blk(BlkLet(ld, Return(Idn(IdnUse(i))))))
        process(program, i, optTypeValue, optAliasedType, config)
    }

    /**
     * Process a user-entered function definition binding.
     */
    def processDef(i : String, fd : Def, optTypeValue : Option[Expression], optAliasedType : Option[Expression], config : Config) : Unit = {
        val program = Program(Blk(BlkDef(Defs(Vector(fd)), Return(Idn(IdnUse(i))))))
        process(program, i, optTypeValue, optAliasedType, config)
    }

    /**
     * Process a user-entered identifier expression.
     */
    def processIdn(i : String, e : Expression, optTypeValue : Option[Expression], optAliasedType : Option[Expression], config : Config) : Unit = {
        val program = Program(e)
        process(program, i, optTypeValue, optAliasedType, config)
    }

    /**
     * Process the AST from the user's entered text.
     */
    def process(
        program : Program,
        i : String,
        optTypeValue : Option[Expression],
        optAliasedType : Option[Expression],
        config : Config
    ) : Unit

    /**
     * Execute a REPL line. If the type of the line is Type, just
     * print that fact, otherwise used eval to do the actual evaluation.
     */
    def execute(
        i : String,
        optTypeValue : Option[Expression],
        optAliasedType : Option[Expression],
        config : Config,
        eval : => Unit
    ) =
        optAliasedType match {
            case Some(`typT`) =>
                output(i, optTypeValue, optAliasedType, None, config)
            case _ =>
                eval
        }

    /**
     * Output in the REPL with name, type and optional result.
     */
    def output(
        i : String,
        optTypeValue : Option[Expression],
        optAliasedType : Option[Expression],
        optResult : Option[OutputValueR],
        config : Config
    ) : Unit = {
        val value =
            optAliasedType match {
                case Some(`typT`) =>
                    optTypeValue match {
                        case Some(typeValue) =>
                            s" = ${show(typeValue)}"
                        case None =>
                            ""
                    }
                case _ =>
                    optResult match {
                        case Some(result) =>
                            s" = ${showRuntimeValue(result)}"
                        case None =>
                            ""
                    }
            }
        val tipe = optAliasedType.map(t => s" : ${show(t)}").getOrElse("")
        config.output().emitln(s"$i$tipe$value")
    }

    /**
     * Extractor for commands, splits the line into separate words.
     */
    object Command {
        def unapply(line : String) : Option[Seq[String]] = {
            Some((line.trim split ' ').toIndexedSeq)
        }
    }

}
