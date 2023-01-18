/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019-2023 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.cooma.truffle

import org.bitbucket.inkytonik.cooma.CoomaException.getUnhandledMessage
import org.bitbucket.inkytonik.cooma._
import org.graalvm.polyglot.PolyglotException

import scala.util.Try

trait TruffleREPL extends REPL {

  self: Compiler with TruffleBackend =>

  def driver: TruffleDriver

  import org.bitbucket.inkytonik.cooma.Config
  import org.bitbucket.inkytonik.cooma.CoomaParserSyntax.{
    Expression,
    Program,
    Type
  }
  import org.bitbucket.inkytonik.cooma.PrettyPrinter.format
  import org.graalvm.polyglot.Context

  var currentDynamicEnv: Context = _

  override def initialise(config: Config): Unit = {
    currentDynamicEnv = Context.newBuilder(CoomaConstants.ID).build()
    super.initialise(config)
  }

  def process(
      program: Program,
      i: String,
      optTypeValue: Option[Expression],
      optAliasedType: Option[Type],
      config: Config,
      analyser: SemanticAnalyser
  ): Unit = {
    driver.setAnalyser(analyser)
    execute(
      i,
      optTypeValue,
      optAliasedType,
      config, {
        val line = format(program).layout
        Try(currentDynamicEnv.eval(CoomaConstants.ID, line)).toEither match {
          case Right(value) =>
            output(i, optTypeValue, optAliasedType, Some(value), config)
          case Left(e: PolyglotException) if e.isGuestException =>
            config.output().emitln(e.getMessage)
          case Left(e) =>
            config.output().emitln(getUnhandledMessage(e))
        }
      }
    )
  }

}

object TruffleReplFrontendHolder {
  def repl(config: Config, td: TruffleDriver): REPL with Compiler with Backend =
    new TruffleBackend(config) with TruffleREPL with Compiler {
      override val driver = td
    }
}
