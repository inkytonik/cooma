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

    import org.bitbucket.inkytonik.cooma.CoomaParserPrettyPrinter.{any, layout}

    override def run(config : Config) {
        if (config.filenames().isEmpty) {
            val repl = createREPL(config)
            repl.driver(config.args)
        } else
            super.run(config)
    }

    override def createREPL(config : Config) : REPL with Compiler with Backend = {
        new ReferenceBackend(config) with REPL with Compiler
    }

    override def process(source : Source, program : CoomaParserSyntax.Program, config : Config) : Unit = {
        val system = new ReferenceBackend(config) with Compiler
        val term = system.compileCommand(program)
        if (config.irPrint())
            config.output().emitln(system.showTerm(term))
        if (config.irASTPrint())
            config.output().emitln(layout(any(term), 5))
        val args = config.filenames().tail
        system.interpret(term, args, config)
    }

}
