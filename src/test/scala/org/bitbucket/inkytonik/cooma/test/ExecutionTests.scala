package org.bitbucket.inkytonik.cooma.test

import java.io.{ByteArrayOutputStream, PrintStream}

import org.bitbucket.inkytonik.cooma.CoomaParserSyntax.{ASTNode, Program}
import org.bitbucket.inkytonik.cooma.backend.ReferenceBackend
import org.bitbucket.inkytonik.cooma.truffle.{TruffleBackend, TruffleDriver, TruffleFrontend, TruffleREPL}
import org.bitbucket.inkytonik.cooma.{Backend, Compiler, Config, Driver, Frontend, REPL, ReferenceDriver, ReferenceFrontend, ReferenceREPL}
import org.bitbucket.inkytonik.kiama.util.{Source, StringConsole, StringSource, TestCompilerWithConfig}
import org.rogach.scallop.throwError
import org.scalactic.source.Position

import scala.util.{Failure, Success, Try}

trait ExecutionTests extends Driver with TestCompilerWithConfig[ASTNode, Program, Config] {

    case class BackendConfig(name : String, frontend : Frontend, options : Seq[String])

    val truffleOutContent = new ByteArrayOutputStream

    val backends = Seq(
        BackendConfig("Reference", new ReferenceFrontend, Seq()),
        BackendConfig("GraalVM", new TruffleFrontend(out = new PrintStream(truffleOutContent)), Seq("-g"))
    )

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
        result3 shouldBe s"""res0 : $tipe = $answer\n"""
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

    override def createREPL(config : Config) : REPL with Compiler with Backend = {
        val repl =
            if (config.graalVM())
                new TruffleBackend(config) with TruffleREPL with Compiler
            else {
                val source = StringSource("")
                new ReferenceBackend(this, source, config) with ReferenceREPL with Compiler
            }
        repl.initialise()
        repl
    }

    override def process(source : Source, prog : Program, config : Config) : Unit = {
        val frontend =
            if (config.graalVM()) new TruffleDriver
            else new ReferenceDriver
        frontend.process(source, prog, config)
    }

    override def testdriver(config : Config) : Unit = {
        if (config.graalVM())
            new TruffleFrontend().interpret(config)
        else
            super.testdriver(config)
    }

}
