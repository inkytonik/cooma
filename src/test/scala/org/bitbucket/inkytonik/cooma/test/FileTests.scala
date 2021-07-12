package org.bitbucket.inkytonik.cooma.test

import org.bitbucket.inkytonik.cooma.CoomaParserSyntax.{ASTNode, Program}
import org.bitbucket.inkytonik.cooma.truffle.{TruffleDriver, TruffleFrontend}
import org.bitbucket.inkytonik.cooma.{Backend, Compiler, Config, REPL, REPLDriver, ReferenceDriver}
import org.bitbucket.inkytonik.kiama.util.{Source, TestCompilerWithConfig}
import org.rogach.scallop.throwError

trait FileTests extends REPLDriver with TestCompilerWithConfig[ASTNode, Program, Config] {

    override def createConfig(args : Seq[String]) : Config = {
        // set Scallop so that errors don't just exit the process
        val saveThrowError = throwError.value
        throwError.value = true
        val config = super.createConfig(args)
        config.verify()
        throwError.value = saveThrowError
        config
    }

    def createDriver(config : Config) : REPLDriver =
        if (config.graalVM())
            new TruffleDriver
        else
            new ReferenceDriver

    override def createREPL(config : Config) : REPL with Compiler with Backend = {
        val driver = createDriver(config)
        val repl = driver.createREPL(config)
        repl.initialise(config)
        repl
    }

    override def process(source : Source, prog : Program, config : Config) : Unit = {
        val driver = createDriver(config)
        driver.process(source, prog, config)
    }

    override def testdriver(config : Config) : Unit = {
        if (config.graalVM())
            new TruffleFrontend().interpret(config)
        else
            super.testdriver(config)
    }

}
