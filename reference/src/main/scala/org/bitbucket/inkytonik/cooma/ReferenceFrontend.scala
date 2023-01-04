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
import org.bitbucket.inkytonik.kiama.util.Source

class ReferenceFrontend extends Frontend {

  val driver = new ReferenceDriver()

  override def interpret(config: Config): Unit = {
    driver.run(config)
  }

  /** This method is mainly built for the tests that execute cooma strings
    * instead of cooma files.
    */
  override def interpret(
      programName: String,
      program: String,
      config: Config
  ): Unit = {
    driver.compileString(programName, program, config)
  }

}

class ReferenceDriver extends REPLDriver {

  import org.bitbucket.inkytonik.cooma.CoomaParserSyntax.Program
  import org.bitbucket.inkytonik.cooma.PrettyPrinter.{
    any,
    format => coomaFormat,
    layout,
    pretty
  }
  import org.bitbucket.inkytonik.kiama.util.StringSource

  override def run(config: Config): Unit = {
    if (config.server()) {
      launch(config)
    } else if (config.filenames().isEmpty) {
      val repl = createREPL(config)
      repl.driver(config.args.toIndexedSeq)
    } else
      super.run(config)
  }

  override def createREPL(config: Config): REPL with Compiler with Backend = {
    new ReferenceBackend(this, StringSource(""), config)
      with ReferenceREPL
      with Compiler
  }

  override def process(
      source: Source,
      program: Program,
      config: Config
  ): Unit = {
    val system = new ReferenceBackend(this, source, config) with Compiler
    val term = system.compileCommand(program, positions, getAnalyser(source))
    if (config.irPrint())
      config.output().emitln(system.showTerm(term))
    if (config.irASTPrint())
      config.output().emitln(layout(any(term), 5))
    if (config.server()) {
      if (settingBool("showIR"))
        publishProduct(source, "IR", "IR", coomaFormat(term, 5))
      if (settingBool("showIRTree"))
        publishProduct(source, "IRtree", "scala", pretty(any(term), 5))
    }
    val args =
      if (config.server())
        config.filenames()
      else
        config.filenames().tail
    if (!config.usage())
      system.interpret(term, args, config)
  }

}
