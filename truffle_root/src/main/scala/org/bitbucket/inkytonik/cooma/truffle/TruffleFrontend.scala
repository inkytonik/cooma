package org.bitbucket.inkytonik.cooma.truffle

import java.io.{InputStream, PrintStream}

import org.bitbucket.inkytonik.cooma.{CoomaConstants, Frontend}

class TruffleFrontend(in : InputStream = System.in, out : PrintStream = System.out) extends Frontend {

    import org.bitbucket.inkytonik.cooma.Config
    import org.graalvm.polyglot.{Context, PolyglotException, Value}

    /**
     * Main entry point, where a cooma file is provided to run in the config.
     * @param config
     */
    override def interpret(config : Config) : Unit =
        if (config.filenames().isEmpty) {
            val repl = new TruffleDriver().createREPL(config)
            repl.driver(config.args.toIndexedSeq)
        } else {
            interpret("", config)
        }

    override def interpret(programName : String, program : String, config : Config) : Unit =
        interpret(program, config)

    private def interpret(program : String, config : Config) : Unit =
        try {
            val context = createContext(config)
            val result = context.eval(CoomaConstants.ID, program)
            printAndClose(config, context, result)
        } catch {
            case e : PolyglotException =>
                config.output().emit(e.getMessage)
        }

    private def printAndClose(config : Config, context : Context, result : Value) = {
        if (CoomaLanguage.Type.Error.value.equals(result.getMetaObject.toString))
            config.output().emitln(result)
        else if (config.resultPrint())
            config.output().emitln(result)
        context.close()
    }

    private def createContext(config : Config) =
        Context.newBuilder(CoomaConstants.ID)
            .out(out).in(in).arguments(CoomaConstants.ID, config.args.toArray)
            .build()

}
