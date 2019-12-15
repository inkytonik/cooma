package org.bitbucket.inkytonik.cooma

import org.bitbucket.inkytonik.cooma.backend.ReferenceBackend

trait ReferenceREPL extends REPL {

    self : Compiler with ReferenceBackend =>

    import org.bitbucket.inkytonik.cooma.CoomaParserSyntax.{Expression, Program}
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinter.{any, layout}

    var currentDynamicEnv : Env = _

    override def initialise() : Unit = {
        super.initialise()
        currentDynamicEnv = emptyEnv
    }

    def process(
        program : Program,
        i : String,
        optTypeValue : Option[Expression],
        aliasedType : Expression,
        config : Config
    ) : Unit = {
        val term = compileStandalone(program)

        if (config.irPrint())
            config.output().emitln(showTerm(term))
        if (config.irASTPrint())
            config.output().emitln(layout(any(term), 5))

        execute(i, optTypeValue, aliasedType, config, {
            val args = config.filenames()
            val result = interpret(term, currentDynamicEnv, args, config)
            isErrR(result) match {
                case Some(_) =>
                    errorOutput(Some(result), config)
                case None => {
                    currentDynamicEnv = consEnv(currentDynamicEnv, i, result)
                    output(i, optTypeValue, aliasedType, Some(result), config)
                }
            }
        })
    }

}
