package org.bitbucket.inkytonik.cooma.truffle

import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
import org.bitbucket.inkytonik.cooma.{Backend, Compiler, Config, CoomaParser, REPL}
import org.bitbucket.inkytonik.kiama.util.{Console, Source, StringConsole, StringSource}

import scala.collection.mutable.ListBuffer

trait TruffleREPL extends REPL {

    self : Compiler with GraalVMBackend =>

    /**
     * Process a line of user input.
     */
    override def processline(source : Source, console : Console, config : Config) : Option[Config] = {

        source.content match {
            case Command(Seq(":help")) =>
                help(config)
            case Command(Seq(":lines")) =>
                processconsole(new StringConsole(getLines(console)), "", config)
            case Command(Seq(":paste")) =>
                enterlinePolyglot(config, getLines(console))
            case line =>
                enterlinePolyglot(config, line)
            case Command(Seq(":quit")) =>
                return None
        }
        Some(config)
    }

    def help(config : Config) {
        config.output().emit("""
							   |exp                    evaluate exp, print value
							   |val x = exp            add new value definition
							   |def f(x : Int) = exp   add new function definition
							   |:help                  print this message
							   |:lines                 enter multiple separate input lines until :end
							   |:paste                 enter single multi-line input until :end
							   |:quit                  quit the REPL (also Control-D)
							   |""".stripMargin)
    }

    def getLines(console : Console) : String = {
        val buf = ListBuffer[String]()
        var line = console.readLine("")
        while (line.trim != ":end") {
            buf.append(line + "\n")
            line = console.readLine("")
        }
        buf.mkString
    }

    /*
	 * Embed an entry in a program and process it.
	 */
    def processEntry(config : Config, input : REPLInput) =
        input match {

            case REPLExpression(e) =>
                val i = s"res$nResults"
                nResults = nResults + 1
                process(Program(Blk(LetVal(Val(i, e), Return(Var(i))))), i, true, config)

            case REPLDef(fd @ Def(i, _, _)) =>
                process(
                    Program(Blk(LetFun(Vector(fd), Return(Var(i))))),
                    i, false, config
                )

            case REPLVal(Val(i, e)) =>
                process(Program(Blk(LetVal(Val(i, e), Return(Var(i))))), i, true, config)

        }

    def enterlinePolyglot(config : Config, line : String) : Unit = {
        val result = currentDynamicEnv.eval(CoomaConstants.ID, line)
    //
    //		if (config.printValue)
    //			config.output().emitln(s"$i = ${showRuntimeValue(result)}")
    //		else
        config.output().emitln(result)
    }

    /*
	 * Enter a REPL input line by processing it, ignoring whitespace lines.
	 */
    def enterline(config : Config, line : String) {

        val source = new StringSource(line)
        val p = new CoomaParser(source, positions)
        val pr = p.pWhitespace(0)
        if (!pr.hasValue) {
            val pr = p.pREPLInput(0)
            if (pr.hasValue)
                processEntry(config, p.value(pr).asInstanceOf[REPLInput])
            else
                config.output().emitln(p.formatParseError(pr.parseError, false))
        }
    }

    def compiledRepl() : Term = truffleNode

}

object TruffleReplHolder {
    def repl(config : Config) : REPL with Compiler with Backend = new GraalVMBackend(config) with TruffleREPL with Compiler
}
