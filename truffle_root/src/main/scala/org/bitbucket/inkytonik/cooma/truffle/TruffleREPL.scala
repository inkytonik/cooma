/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.cooma.truffle

import org.bitbucket.inkytonik.cooma._

trait TruffleREPL extends REPL {

    self : Compiler with TruffleBackend =>

    import org.bitbucket.inkytonik.cooma.Config
    import org.bitbucket.inkytonik.cooma.PrettyPrinter.format
    import org.bitbucket.inkytonik.cooma.CoomaParserSyntax.{Expression, Program}
    import org.graalvm.polyglot.Context

    var currentDynamicEnv : Context = _

    override def initialise(config : Config) : Unit = {
        currentDynamicEnv = Context.newBuilder(CoomaConstants.ID).build()
        super.initialise(config)
    }

    def process(
        program : Program,
        i : String,
        optTypeValue : Option[Expression],
        optAliasedType : Option[Expression],
        config : Config
    ) : Unit = {
        execute(i, optTypeValue, optAliasedType, config, {
            val line = format(program).layout
            val value = currentDynamicEnv.eval(CoomaConstants.ID, line)
            if (CoomaLanguage.Type.Error.value.equals(value.getMetaObject.toString))
                errorOutput(Some(value), config)
            else
                output(i, optTypeValue, optAliasedType, Some(value), config)
        })
    }

}

object TruffleReplFrontendHolder {
    def repl(config : Config) : REPL with Compiler with Backend =
        new TruffleBackend(config) with TruffleREPL with Compiler
}
