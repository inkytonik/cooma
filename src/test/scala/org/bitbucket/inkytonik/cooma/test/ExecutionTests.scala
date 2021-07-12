package org.bitbucket.inkytonik.cooma.test

import org.bitbucket.inkytonik.cooma.CoomaParserSyntax.Program
import org.bitbucket.inkytonik.cooma.test.BackendConfig._
import org.bitbucket.inkytonik.cooma.truffle.TruffleDriver
import org.bitbucket.inkytonik.cooma.{Backend, Compiler, Config, Main, REPL, REPLDriver, ReferenceDriver}
import org.bitbucket.inkytonik.kiama.util.{Source, StringConsole}
import org.rogach.scallop.throwError
import org.scalactic.source.Position
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should

import scala.util.{Failure, Success, Try}

trait ExecutionTests extends REPLDriver with AnyFunSuiteLike with should.Matchers {

    def test(name : String)(f : BackendConfig => Any)(implicit pos : Position) : Unit =
        for (bc <- backends) super.test(f"[${bc.name}] $name")(f(bc))

    def runTest(tester : Config => Unit, allArgs : Seq[String]) : String = {
        val config = createConfig(allArgs)
        Try(tester(config)) match {
            case Success(()) =>
                if (config.graalVM() && truffleOutContent.size() > 0) {
                    val result = truffleOutContent.toString
                    truffleOutContent.reset()
                    result
                } else
                    config.stringEmitter.result()
            case Failure(e) =>
                info("failed with an exception")
                throw e
        }
    }

    def runString(name : String, program : String, options : Seq[String])(implicit bc : BackendConfig) : String = {
        val allArgs = Seq("--Koutput", "string") ++ bc.options ++ options :+ "test.cooma"
        runTest(bc.frontend.interpret(name, program, _), allArgs)
    }

    def runFile(program : String, options : Seq[String], args : Seq[String])(implicit bc : BackendConfig) : String = {
        val allArgs = Seq("--Koutput", "string") ++ bc.options ++ options ++ (program +: args)
        runTest(bc.frontend.interpret, allArgs)
    }

    def runMain(program : String, options : Seq[String], args : Seq[String])(implicit bc : BackendConfig) : String = {
        val allArgs = Seq("--Koutput", "string") ++ bc.options ++ options ++ (program +: args)
        runTest(Main.runMain, allArgs)
    }

    def runREPLTest(cmd : String, input : String, options : Seq[String])(implicit bc : BackendConfig) : String = {
        val allArgs = Seq("--Koutput", "string") ++ bc.options ++ options
        val config = createConfig(allArgs)
        val replInput = if (input.indexOf('\n') == -1) input else s"$cmd\n$input\n:end"
        val console = new StringConsole(replInput)
        val repl = createREPL(config)
        runTest(repl.processconsole(console, "dummy", _), allArgs)
    }

    def runREPLOnLine(input : String, options : Seq[String])(implicit bc : BackendConfig) : String =
        runREPLTest(":paste", input, options)

    def runREPLOnLines(input : String, options : Seq[String])(implicit bc : BackendConfig) : String =
        runREPLTest(":lines", input, options)

    def runPrimTest(name : String, args : String, tipe : String, answer : String)(implicit bc : BackendConfig) : Unit = {
        val code = s"$name($args)"
        val result1 = runString(name, code, Seq())
        result1 shouldBe ""
        val result2 = runString(name, code, Seq("-r"))
        result2 shouldBe s"$answer\n"
        val result3 = runREPLOnLine(code, Seq())
        result3 shouldBe s"""res0$tipe = $answer\n"""
    }

    def runBadPrimTest(name : String, args : String, error : String)(implicit bc : BackendConfig) : Unit = {
        val code = s"$name($args)"
        val result1 = runString(name, code, Seq())
        result1 shouldBe s"$error\n"
        val result2 = runString(name, code, Seq("-r"))
        result2 shouldBe s"$error\n"
        val result3 = runREPLOnLine(code, Seq())
        result3 shouldBe s"$error\n"
    }

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

}
