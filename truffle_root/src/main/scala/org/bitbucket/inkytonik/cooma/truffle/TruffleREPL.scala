package org.bitbucket.inkytonik.cooma.truffle

import org.bitbucket.inkytonik.cooma.{Backend, Config, REPLFrontend}

trait TruffleREPLFrontend extends REPLFrontend {

	self: GraalVMBackend =>

	def processProgram(config: Config, line: String, i: String, printValue: Boolean): Unit = {
		val result = currentDynamicEnv.eval(CoomaConstants.ID, line)
		if (printValue)
			config.output().emitln(s"$i = ${showRuntimeValue(result)}")
		else
			config.output().emitln(i)
		config.output().emitln(result)
	}

	def compiledRepl(): Term = truffleNode

}

object TruffleReplFrontendHolder {
	def repl(config: Config): REPLFrontend with Backend = new GraalVMBackend(config) with TruffleREPLFrontend
}
