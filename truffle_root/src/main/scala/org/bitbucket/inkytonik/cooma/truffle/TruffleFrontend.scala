package org.bitbucket.inkytonik.cooma.truffle
import java.io.{InputStream, PrintStream}

import org.bitbucket.inkytonik.cooma.{Config, Frontend}
import org.graalvm.polyglot.{Context, Value}

class TruffleFrontend(in : InputStream = System.in, out : PrintStream = System.out) extends Frontend {

    override def interpret(config : Config) : Unit = {
        val context : Context = createContext(config)
        printAndClose(config, context, context.eval(CoomaConstants.ID, ""))
    }

    private def printAndClose(config : Config, context : Context, result : Value) = {
        if (CoomaLanguage.Type.Error.value.equals(result.getMetaObject.toString)) {
            config.output().emitln(result)
        }
        if (config.resultPrint()) config.output().emitln(result)
        context.close()
    }

    override def interpret(programName : String, program : String, config : Config) : Unit = {
        val context : Context = createContext(config)
        printAndClose(config, context, context.eval(CoomaConstants.ID, program))
    }

    private def createContext(config : Config) = {
        Context.newBuilder(CoomaConstants.ID)
            .out(out).in(in).arguments(CoomaConstants.ID, config.args.toArray)
            .build()
    }
}
