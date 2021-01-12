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
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinter.{any, layout}

    var currentDynamicEnv : Env = _

    override def initialise() : Unit = {
        super.initialise()
        currentDynamicEnv = emptyEnv
    }

    def process(
        program : Program,
        i : String,
        optTypeValue : Option[Expression],
        aliasedType : Expression,
        config : Config
    ) : Unit = {
        val term = compileStandalone(program)

        if (config.irPrint())
            config.output().emitln(showTerm(term))
        if (config.irASTPrint())
            config.output().emitln(layout(any(term), 5))

        execute(i, optTypeValue, aliasedType, config, {
            val args = config.filenames()
            val result = interpret(term, currentDynamicEnv, args, config)
            isErrR(result) match {
                case Some(_) =>
                    errorOutput(Some(result), config)
                case None => {
                    currentDynamicEnv = consEnv(currentDynamicEnv, i, result)
                    output(i, optTypeValue, aliasedType, Some(result), config)
                }
            }
        })
    }

}
