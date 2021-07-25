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

import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaTermNode
import org.bitbucket.inkytonik.cooma.{Config, CoomaParserSyntax, SemanticAnalyser}

class TruffleCompiler(val config : Config, analyser : SemanticAnalyser) {

    import org.bitbucket.inkytonik.cooma.Compiler
    import org.bitbucket.inkytonik.kiama.util.Positions

    val backendMixin = new TruffleBackend(config) with Compiler

    def compileCommand(prog : CoomaParserSyntax.Program) : CoomaTermNode = {
        backendMixin.compileCommand(prog, new Positions, analyser)
    }

    def showTerm(t : CoomaTermNode) : String = backendMixin.showTerm(t)

}
