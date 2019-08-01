package org.bitbucket.inkytonik.cooma.truffle
import org.bitbucket.inkytonik.cooma.{Config, Frontend}
import org.graalvm.polyglot
import org.graalvm.polyglot.Context

class TruffleFrontend extends Frontend {

    override def interpret(config : Config) : Unit = {

        val context = Context.newBuilder(CoomaConstants.ID)
            .arguments(CoomaConstants.ID, config.args.toArray)
            .build()

        val result : polyglot.Value = context.eval(CoomaConstants.ID, "")

        if (CoomaLanguage.Type.Error.value.equals(result.getMetaObject.toString)) {
            config.output().emitln(result)
        }
        if (config.resultPrint()) config.output().emitln(result)

        context.close()
    }
}
