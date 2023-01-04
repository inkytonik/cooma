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

import org.bitbucket.inkytonik.cooma.backend.ReferenceBackend

trait ReferenceREPL extends REPL {

  self: Compiler with ReferenceBackend =>

  import org.bitbucket.inkytonik.cooma.CoomaParserSyntax.{Expression, Program}
  import org.bitbucket.inkytonik.cooma.PrettyPrinter.{any, layout}

  var currentDynamicEnv: Env = _

  override def initialise(config: Config): Unit = {
    currentDynamicEnv = preludeDynamicEnv(config).left.map { msg =>
      config.output().emitln(msg)
      sys.exit(-1)
    }.merge
    super.initialise(config)
  }

  def process(
      program: Program,
      i: String,
      optTypeValue: Option[Expression],
      optAliasedType: Option[Expression],
      config: Config,
      analyser: SemanticAnalyser
  ): Unit = {
    val term = compileStandalone(program, positions, analyser)

    if (config.irPrint())
      config.output().emitln(showTerm(term))
    if (config.irASTPrint())
      config.output().emitln(layout(any(term), 5))

    execute(
      i,
      optTypeValue,
      optAliasedType,
      config, {
        val args = config.filenames()
        interpret(term, currentDynamicEnv, args, config) match {
          case Right(Result(_, value)) =>
            currentDynamicEnv = ConsVE(i, value, currentDynamicEnv)
            output(i, optTypeValue, optAliasedType, Some(value), config)
          case Left(msg) =>
            config.output().emitln(msg)
        }
      }
    )
  }

}
