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

    self : Compiler with ReferenceBackend =>

    import org.bitbucket.inkytonik.cooma.CoomaParserSyntax.{Expression, Program}
    import org.bitbucket.inkytonik.cooma.PrettyPrinter.{any, layout}

    var currentDynamicEnv : Env = _

    override def initialise(config : Config) : Unit = {
        currentDynamicEnv = preludeDynamicEnv(config)
        super.initialise(config)
    }

    def process(
        program : Program,
        i : String,
        optTypeValue : Option[Expression],
        optAliasedType : Option[Expression],
        config : Config
    ) : Unit = {
        val term = compileStandalone(program, positions)

        if (config.irPrint())
            config.output().emitln(showTerm(term))
        if (config.irASTPrint())
            config.output().emitln(layout(any(term), 5))

        execute(i, optTypeValue, optAliasedType, config, {
            val args = config.filenames()
            val result = interpret(term, currentDynamicEnv, args, config)
            val value = result.value
            isErrR(value) match {
                case Some(_) =>
                    errorOutput(Some(value), config)
                case None =>
                    currentDynamicEnv = ConsVE(i, value, currentDynamicEnv)
                    output(i, optTypeValue, optAliasedType, Some(value), config)
            }
        })
    }

}
