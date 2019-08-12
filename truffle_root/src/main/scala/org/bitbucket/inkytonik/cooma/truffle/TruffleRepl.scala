package org.bitbucket.inkytonik.cooma.truffle

import org.bitbucket.inkytonik.cooma.truffle.scala.GraalVMBackend
import org.bitbucket.inkytonik.cooma.{Backend, Compiler, Config, REPL}

object TruffleRepl {
    def repl(config : Config) : REPL with Compiler with Backend = new GraalVMBackend(config) with REPL with Compiler
}
