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

import org.bitbucket.inkytonik.kiama.util.REPLConfig

class Config(args: Seq[String]) extends REPLConfig(args) {

  version(s"${BuildInfo.name} ${BuildInfo.version}\n")

  banner("Options:\n")

  lazy val coomaASTPrint = opt[Boolean](
    "coomaASTPrint",
    short = 'C',
    descr = "Print the AST of the Cooma program (default: false)",
    default = Some(false)
  )

  lazy val desugaredASTPrint = opt[Boolean](
    "desugaredASTPrint",
    short = 'D',
    descr = "Print the AST of the desugared Cooma program (default: false)",
    default = Some(false)
  )

  lazy val graalVM = opt[Boolean](
    "graallvm",
    short = 'g',
    descr = "Use the Graal VM backend (default: false)",
    default = Some(false)
  )

  lazy val irPrint = opt[Boolean](
    "irPrint",
    short = 'i',
    descr = "Print the intermediate representation (default: false)",
    default = Some(false)
  )

  lazy val irASTPrint = opt[Boolean](
    "irASTPrint",
    short = 'I',
    descr = "Print the AST of the intermediate representation (default: false)",
    default = Some(false)
  )

  lazy val preludePath = opt[String](
    "preludePath",
    short = 'p',
    descr = "The path to the prelude code (default: prelude/prelude.cooma)",
    default = Some("prelude/prelude.cooma")
  )

  lazy val compilePrelude = opt[Boolean](
    "compilePrelude",
    short = 'P',
    descr = "Compile the Cooma program as a prelude (default: false)",
    default = Some(false)
  )

  lazy val noPrelude = opt[Boolean](
    "noPrelude",
    short = 'Q',
    descr = "Don't use a prelude (default: false)",
    default = Some(false)
  )

  lazy val resultPrint = opt[Boolean](
    "resultPrint",
    short = 'r',
    descr = "Print the result value in compiler mode (default: false)",
    default = Some(false)
  )

  lazy val trace = opt[Boolean](
    "trace",
    short = 'T',
    descr = "Trace the IR execution (default: false)",
    default = Some(false)
  )

  lazy val typePrint = opt[Boolean](
    "typePrint",
    short = 't',
    descr = "Print the program type in compiler mode (default: false)",
    default = Some(false)
  )

  lazy val usage = opt[Boolean](
    "usage",
    short = 'u',
    descr = "Print the program's usage message (default: false)",
    default = Some(false)
  )

  override def hashCode: Int = args.hashCode

  override def equals(o: Any): Boolean =
    o match {
      case other: Config => this.args == other.args
      case _             => false
    }

}
