package org.bitbucket.inkytonik.cooma.truffle

import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
import org.bitbucket.inkytonik.cooma.{Backend, Compiler, Config, CoomaParserPrettyPrinter, REPL}

trait TruffleREPL extends REPL {

    self : Compiler with TruffleBackend =>

    /*
	* Embed an entry in a program and process it.
	*/
    override def processEntry(config : Config, input : REPLInput) = {
        val res = s"res$nResults"
        input match {
            case REPLExpression(e) =>
                nResults = nResults + 1
                processProgram(config, CoomaParserPrettyPrinter.format(
                    Program(Blk(LetVal(Val(IdnDef(res), e), Return(Var(IdnUse(res)))))),
                    5
                ).layout, res, printValue = true)

            case REPLDef(fd @ Def(IdnDef(i), _, _)) =>
                processProgram(config, CoomaParserPrettyPrinter.format(
                    Program(Blk(LetFun(Vector(fd), Return(Var(IdnUse(i)))))),
                    5
                ).layout, i, printValue = false)

            case REPLVal(Val(IdnDef(i), e)) =>
                processProgram(config, CoomaParserPrettyPrinter.format(
                    Program(Blk(LetVal(Val(IdnDef(i), e), Return(Var(IdnUse(i)))))),
                    5
                ).layout, i, printValue = true)
        }
    }

    def processProgram(config : Config, line : String, i : String, printValue : Boolean) : Unit = {
        val result = currentDynamicEnv.eval(CoomaConstants.ID, line)
        if (printValue)
            config.output().emitln(s"$i = ${showRuntimeValue(result)}")
        else
            config.output().emitln(i)
    }

}

object TruffleReplFrontendHolder {
    def repl(config : Config) : REPL with Compiler with Backend = new TruffleBackend(config) with TruffleREPL with Compiler
}
