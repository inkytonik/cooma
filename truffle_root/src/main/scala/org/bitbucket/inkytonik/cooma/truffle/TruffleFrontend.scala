package org.bitbucket.inkytonik.cooma.truffle
import java.io.{InputStream, PrintStream}

import org.bitbucket.inkytonik.cooma.{Config, Frontend}
import org.graalvm.polyglot.{Context, Value}

class TruffleFrontend(in : InputStream = System.in, out : PrintStream = System.out) extends Frontend {

    /**
     * Main entry point, where a cooma file is provided to run in the config or, if empty
     * @param config
     */
    override def interpret(config : Config) : Unit = {
        if (config.filenames().isEmpty) {
            val repl = new TruffleDriver().createREPL(config)
            repl.driver(config.args)
        } else {
            val context : Context = createContext(config)
            printAndClose(config, context, context.eval(CoomaConstants.ID, ""))
        }
    }

    override def interpret(programName : String, program : String, config : Config) : Unit = {
        val context : Context = createContext(config)
        printAndClose(config, context, context.eval(CoomaConstants.ID, program))
    }

    private def printAndClose(config : Config, context : Context, result : Value) = {
        if (CoomaLanguage.Type.Error.value.equals(result.getMetaObject.toString)) {
            config.output().emitln(result)
        }
        if (config.resultPrint()) config.output().emitln(result)
        context.close()
    }

    private def createContext(config : Config) = {
        Context.newBuilder(CoomaConstants.ID)
            .out(out).in(in).arguments(CoomaConstants.ID, config.args.toArray)
            .build()
    }
}
