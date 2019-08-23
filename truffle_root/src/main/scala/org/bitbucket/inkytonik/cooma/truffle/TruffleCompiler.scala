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

