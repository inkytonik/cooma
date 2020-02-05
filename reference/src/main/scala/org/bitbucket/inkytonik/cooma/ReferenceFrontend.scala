package org.bitbucket.inkytonik.cooma

import org.bitbucket.inkytonik.cooma.backend.ReferenceBackend
import org.bitbucket.inkytonik.kiama.util.Source

class ReferenceFrontend extends Frontend {

    val driver = new ReferenceDriver()

    override def interpret(config : Config) : Unit = {
        driver.run(config)
    }

    /**
     * This method is mainly built for the tests that execute cooma strings instead of
     * cooma files.
     */
    override def interpret(programName : String, program : String, config : Config) : Unit = {
        driver.compileString(programName, program, config)
    }

}

class ReferenceDriver extends Driver {

    import org.bitbucket.inkytonik.cooma.CoomaParserPrettyPrinter.{any, layout, pretty}
    import org.bitbucket.inkytonik.kiama.util.StringSource

    override def run(config : Config) : Unit = {
        if (config.server()) {
            launch(config)
        } else if (config.filenames().isEmpty) {
            val repl = createREPL(config)
            repl.driver(config.args.toIndexedSeq)
        } else
            super.run(config)
    }

    override def createREPL(config : Config) : REPL with Compiler with Backend = {
        new ReferenceBackend(this, StringSource(""), config) with ReferenceREPL with Compiler
    }

    override def process(source : Source, program : CoomaParserSyntax.Program, config : Config) : Unit = {
        val system = new ReferenceBackend(this, source, config) with Compiler
        val term = system.compileCommand(program)
        if (config.irPrint())
            config.output().emitln(system.showTerm(term))
        if (config.irASTPrint())
            config.output().emitln(layout(any(term), 5))
        if (config.server()) {
            if (settingBool("showIR"))
                publishProduct(source, "IR", "IR", system.formatTerm(term, 5))
            if (settingBool("showIRTree"))
                publishProduct(source, "IRtree", "scala", pretty(any(term), 5))
        }
        val args =
            if (config.server())
                config.filenames()
            else
                config.filenames().tail
        if (!config.usage())
            system.interpret(term, args, config)
    }

}
