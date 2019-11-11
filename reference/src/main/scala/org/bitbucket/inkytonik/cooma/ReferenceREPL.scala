package org.bitbucket.inkytonik.cooma

import org.bitbucket.inkytonik.cooma.backend.ReferenceBackend

trait ReferenceREPL extends REPL {

    self : Compiler with ReferenceBackend =>

    import org.bitbucket.inkytonik.cooma.CoomaParserSyntax.{Expression, Program}
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinter.{any, layout}

    var currentDynamicEnv : Env = _

    override def initialise() {
        super.initialise()
        currentDynamicEnv = predefEnv
    }

    def process(
        program : Program,
        i : String,
        tipe : Expression,
        config : Config
    ) {
        val term = compileStandalone(program)

        if (config.irPrint())
            config.output().emitln(showTerm(term))
        if (config.irASTPrint())
            config.output().emitln(layout(any(term), 5))

        execute(i, tipe, config, {
            val args = config.filenames()
            val result = interpret(term, currentDynamicEnv, args, config)
            isErrR(result) match {
                case Some(msg) =>
                    errorOutput(msg, config)
                case None => {
                    currentDynamicEnv = consEnv(currentDynamicEnv, i, result)
                    output(i, tipe, Some(result), config)
                }
            }
        })
    }

}
