package org.bitbucket.inkytonik.cooma

import org.bitbucket.inkytonik.cooma.CoomaParserPrettyPrinter.{any, layout}
import org.bitbucket.inkytonik.cooma.CoomaParserSyntax.Program

trait REPLBackend {

	self : Compiler with Backend =>

	/**
	  * Process the AST from the user's entered text.
	  */
	def process(program : Program, i : String, printValue : Boolean, config : Config) {
		if (config.coomaASTPrint())
			config.output().emitln(layout(any(program), 5))
		val term = compileStandalone(program)

		currentDynamicEnv = repl(currentDynamicEnv, i, printValue, config, term)
	}

}
