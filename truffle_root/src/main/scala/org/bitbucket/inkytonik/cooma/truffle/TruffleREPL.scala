package org.bitbucket.inkytonik.cooma.truffle

import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
import org.bitbucket.inkytonik.cooma.{Backend, Compiler, Config, CoomaParserPrettyPrinter, REPL}

trait TruffleREPL extends REPL {

    self : Compiler with TruffleBackend =>

    import org.bitbucket.inkytonik.cooma.CoomaParserPrettyPrinter.show

    /**
     * Process a user-entered value binding.
     */
    override def processVal(i : String, vd : Val, tipe : Expression, config : Config) {
        processLine(
            CoomaParserPrettyPrinter.format(makeVal(i, vd)).layout,
            i, tipe, config
        )
    }

    /**
     * Process a user-entered function definition binding.
     */
    override def processDef(i : String, fd : Def, tipe : Expression, config : Config) {
        processLine(
            CoomaParserPrettyPrinter.format(makeDef(i, fd)).layout,
            i, tipe, config
        )
    }

    def processLine(
        line : String,
        i : String,
        tipe : Expression,
        config : Config
    ) {
        val result = currentDynamicEnv.eval(CoomaConstants.ID, line)
        config.output().emitln(s"$i : ${show(tipe)} = ${showRuntimeValue(result)}")
    }

}

object TruffleReplFrontendHolder {
    def repl(config : Config) : REPL with Compiler with Backend =
        new TruffleBackend(config) with TruffleREPL with Compiler
}
