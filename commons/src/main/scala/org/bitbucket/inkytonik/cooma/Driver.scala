/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019-2023 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.cooma

import org.bitbucket.inkytonik.cooma.CoomaParserSyntax.{ASTNode, Program}
import org.bitbucket.inkytonik.kiama.util.CompilerBase

abstract class Driver
    extends CompilerBase[ASTNode, Program, Config]
    with Server {

  import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
  import org.bitbucket.inkytonik.cooma.Desugar.desugar
  import org.bitbucket.inkytonik.cooma.PrettyPrinter.{any, layout, pretty, show}
  import org.bitbucket.inkytonik.cooma.SymbolTable.{
    Environment,
    capabilityTypeNames,
    preludeStaticEnv,
    StrT
  }
  import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document
  import org.bitbucket.inkytonik.kiama.relation.{EnsureTree, Tree}
  import org.bitbucket.inkytonik.kiama.util.Messaging.{Messages, noMessages}
  import org.bitbucket.inkytonik.kiama.util.Source

  val name = "cooma"

  def createConfig(args: Seq[String]): Config =
    new Config(args)

  override def driver(args: Seq[String]): Unit = {
    createAndInitConfig(args) match {
      case Left(message) =>
        System.err.println(s"cooma: $message, use --help for options")
      case Right(config) =>
        run(config)
    }
  }

  def isREPL(config: Config) =
    config.filenames().isEmpty

  override def compileFiles(config: Config): Unit = {
    if (!isREPL(config))
      compileFile(config.filenames()(0), config)
  }

  def getAnalyser(source: Source): SemanticAnalyser =
    analysers.getOrElse(
      source,
      throw new RuntimeException(s"no analyser for $source")
    )

  val analysers = scala.collection.mutable.Map[Source, SemanticAnalyser]()

  override def compileSource(source: Source, config: Config): Unit = {
    sources(source.name) = source
    clearSyntacticMessages(source, config)
    makeast(source, config) match {
      case Left(ast) =>
        process(source, ast, config)
      case Right(messages) =>
        clearSemanticMessages(source, config)
        report(source, messages, config)
    }
  }

  override def makeast(
      source: Source,
      config: Config
  ): Either[Program, Messages] = {
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
      val env = preludeStaticEnv(config)
      desugar(program, env, positions).flatMap(TopLevelRewriter(_)) match {
        case Left(messages) =>
          Right(messages)
        case Right(desugaredProgram) =>
          if (config.desugaredASTPrint())
            config.output().emitln(layout(any(desugaredProgram), 5))
          if (config.server())
            publishDesugaredTreeProduct(source, pretty(any(program)))
          checkProgram(desugaredProgram, env, source, config) match {
            case Vector() =>
              Left(desugaredProgram)
            case messages =>
              Right(messages)
          }
      }
    } else
      Right(Vector(p.errorToMessage(pr.parseError)))
  }

  def checkProgram(
      program: Program,
      env: Environment,
      source: Source,
      config: Config
  ): Messages =
    if (!config.server() && config.filenames().isEmpty) {
      noMessages
    } else {
      val tree = new Tree[ASTNode, Program](program, EnsureTree)
      val analyser = new SemanticAnalyser(tree, env)
      analysers.get(source) match {
        case Some(prevAnalyser) =>
          positions.resetAllAt(prevAnalyser.tree.nodes)
        case _ =>
        // Do nothing
      }
      analysers(source) = analyser
      analyser.tipe(tree.root.expression) match {
        case Some(tipe) =>
          if (config.typePrint())
            config.output().emitln(show(tipe))
          if (analyser.errors.isEmpty && config.usage())
            printUsage(program.expression, tipe, config)
        case None =>
          if (config.typePrint())
            config.output().emitln("unknown type")
      }
      analyser.errors
    }

  def printUsage(
      expression: Expression,
      tipe: Expression,
      config: Config
  ): Unit = {

    def printArgument(argument: Argument): Unit = {
      config.output().emit(s"  ${argument.idnDef.identifier}: ")
      def aux(t: Expression): Seq[String] =
        t match {
          case Idn(IdnUse(name)) if capabilityTypeNames(name) =>
            Seq(capabilityDesc(name))
          case Cat(e1, e2) =>
            aux(e1) ++ aux(e2)
          case StrT() =>
            Seq("a string")
          case t =>
            Seq(s"unsupported argument type ${show(t)}")
        }
      val description = aux(argument.expression).mkString(", ")
      config.output().emit(description)
      argument.optStringLit match {
        case Some(doc) =>
          config.output().emitln(s" $doc")
        case None =>
          config.output().emitln("")
      }
    }

    def capabilityDesc(name: String): String =
      name match {
        case "FolderReader" => "a folder reader"
        case "FolderWriter" => "a folder writer"
        case "HttpDelete"   => "a HTTP client (DELETE)"
        case "HttpGet"      => "a HTTP client (GET)"
        case "HttpPost"     => "a HTTP client (POST)"
        case "HttpPut"      => "a HTTP client (PUT)"
        case "Reader"       => "a reader"
        case "Table"        => "a database table"
        case "Writer"       => "a writer"
        case _ =>
          sys.error(s"printCapabilityDesc: unknown capability name $name")
      }

    def printArguments(arguments: Arguments): Unit =
      arguments.optArguments match {
        case Vector() =>
          config.output().emitln("  none")
        case arguments =>
          arguments.map(printArgument)
      }

    def printResultType(resultType: Expression): Unit =
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

  override def format(prog: Program): Document =
    PrettyPrinter.format(prog, 5)

}

abstract class REPLDriver extends Driver {

  def createREPL(config: Config): REPL with Compiler with Backend

}
