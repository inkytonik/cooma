package org.bitbucket.inkytonik.cooma

class ReferenceFrontend extends Frontend {

    override def interpret(config : Config) : Unit = {
        //		val system = new ReferenceBackend(config) with Compiler
        //
        //		//TODO get the source AST somehow
        //
        //		val term = system.compileCommand(prog)
        //		if (config.irPrint())
        //			config.output().emitln(system.showTerm(term))
        //		if (config.irASTPrint())
        //			config.output().emitln(layout(any(term), 5))
        //		val args = config.filenames().tail
        //		system.interpret(term, args, config)
    }
}
