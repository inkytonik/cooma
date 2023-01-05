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

import java.io.{InputStream, PrintStream}

import org.bitbucket.inkytonik.cooma.CoomaException._
import org.bitbucket.inkytonik.cooma.{CoomaConstants, Frontend}
import org.graalvm.polyglot.PolyglotException

import scala.util.Try

class TruffleFrontend(
    in: InputStream = System.in,
    out: PrintStream = System.out
) extends Frontend {

  import org.bitbucket.inkytonik.cooma.Config
  import org.graalvm.polyglot.{Context, Value}

  /** Main entry point, where a cooma file is provided to run in the config.
    * @param config
    */
  override def interpret(config: Config): Unit = {
    if (config.filenames().isEmpty) {
      val repl = new TruffleDriver().createREPL(config)
      repl.driver(config.args.toIndexedSeq)
    } else {
      interpret("", config)
    }
  }

  override def interpret(
      programName: String,
      program: String,
      config: Config
  ): Unit =
    interpret(program, config)

  private def interpret(program: String, config: Config): Unit =
    Try {
      val context = createContext(config)
      val result = context.eval(CoomaConstants.ID, program)
      printAndClose(config, context, result)
    }.toEither.left.foreach {
      case e: PolyglotException if e.isGuestException =>
        config.output().emitln(e.getMessage.trim)
      case e => config.output().emitln(getUnhandledMessage(e))
    }

  private def printAndClose(config: Config, context: Context, result: Value) = {
    if (config.resultPrint())
      config.output().emitln(result)
    context.close()
  }

  private def createContext(config: Config) =
    Context
      .newBuilder(CoomaConstants.ID)
      .out(out)
      .in(in)
      .arguments(CoomaConstants.ID, config.args.toArray)
      .build()
}
