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
import org.bitbucket.inkytonik.cooma.{Config, CoomaParserSyntax}

class TruffleCompiler(val config : Config) {
    import org.bitbucket.inkytonik.cooma.Compiler

    val backendMixin = new TruffleBackend(config) with Compiler

    /**
     * Compile a program that will run as a command with
     * user-supplied command-line arguments.
     */
    def compileCommand(prog : CoomaParserSyntax.Program) : CoomaTermNode = {
        backendMixin.compileCommand(prog)
    }

    /**
     * Custom IR pretty-printer that escapes string terms.
     *
     * @param t
     * @return
     */
    def showTerm(t : CoomaTermNode) : String = backendMixin.showTerm(t)

}
