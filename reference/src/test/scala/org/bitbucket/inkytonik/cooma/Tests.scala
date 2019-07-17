/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.cooma

import org.bitbucket.inkytonik.cooma.CoomaParserSyntax.{ASTNode, Program}
import org.bitbucket.inkytonik.kiama.util.TestCompilerWithConfig

class Tests extends Driver with TestCompilerWithConfig[ASTNode, Program, Config] {

    import java.nio.file.{Files, Paths}

    import org.bitbucket.inkytonik.kiama.util.Filenames.makeTempFilename
    import org.bitbucket.inkytonik.kiama.util.IO.{createFile, deleteFile}
    import org.bitbucket.inkytonik.kiama.util.{FileSource, StringConsole}
    import org.rogach.scallop.throwError

    case class Backend(
        name : String,
        options : Seq[String]
    )

    val backends =
        List(
            Backend("Reference", Seq()),
            Backend("GraalVM", Seq("-g"))
        )

    for (backend <- backends) {

        // Basic tests

        case class BasicTest(
            name : String,
            program : String,
            expectedCompiledResult : String,
            args : Seq[String] = Seq(),
            expectedREPLResult : Option[String] = None
        )

        val basicTests =
            List(
                // Primitive values

                BasicTest(
                    "positive integer",
                    "42",
                    "42"
                ),
                BasicTest(
                    "negative integer",
                    "-182",
                    "-182"
                ),
                BasicTest(
                    "string",
                    """"hello"""",
                    """"hello""""
                ),
                BasicTest(
                    "string with quote",
                    """"hel\"lo"""",
                    """"hel\"lo""""
                ),
                BasicTest(
                    "string with newline",
                    """"hello\n"""",
                    """"hello\n""""
                ),
                BasicTest(
                    "string with escape sequences",
                    """"\b\t\n\f\t\7\15\167"""",
                    """"\b\t\n\f\t\7\rw""""
                ),

                // Rows

                BasicTest(
                    "unit",
                    "{}",
                    "{}"
                ),
                BasicTest(
                    "row (single int field)",
                    "{x = 65}",
                    "{x = 65}"
                ),
                BasicTest(
                    "row (single string field)",
                    """{name = "Harold"}""",
                    """{name = "Harold"}"""
                ),
                BasicTest(
                    "row (two fields)",
                    "{a = 1, b = 2}",
                    "{a = 1, b = 2}"
                ),
                BasicTest(
                    "row (many fields)",
                    """{name = "Bob", age = 24, year = 1998, sex = "F"}""",
                    """{name = "Bob", age = 24, year = 1998, sex = "F"}"""
                ),
                BasicTest(
                    "multi-line row",
                    """{
                        name = "Bob",
                        age = 24
                    }""",
                    """{name = "Bob", age = 24}"""
                ),
                BasicTest(
                    "field select (first of one)",
                    """{s = "Hi"}.s""",
                    """"Hi""""
                ),
                BasicTest(
                    "field select (first of two)",
                    """{s = "Hi", t = 10}.s""",
                    """"Hi""""
                ),
                BasicTest(
                    "field select (second of two)",
                    """{s = "Hi", t = 10}.t""",
                    "10"
                ),
                BasicTest(
                    "field select (many fields)",
                    """{name = "Bob", age = 24, year = 1998, sex = "F"}.sex""",
                    """"F""""
                ),
                BasicTest(
                    "nested field select",
                    "{r = {y = 42}}.r.y",
                    "42"
                ),
                BasicTest(
                    "row concatenation",
                    """{
                        val r = {x = 10, y = 20}
                        val s = {a = "Hi"}
                        r & s
                    }""",
                    """{x = 10, y = 20, a = "Hi"}"""
                ),
                BasicTest(
                    "select from row concatenation (left)",
                    """{
                       val r = {x = 10, y = 20}
                        val s = {a = "Hi"}
                       {r & s}.x
                   }""",
                    "10"
                ),
                BasicTest(
                    "select from row concatenation (right)",
                    """{
                       val r = {x = 10, y = 20}
                       val s = {a = "Hi"}
                        {r & s}.a
                   }""",
                    """"Hi""""
                ),

                // Functions

                BasicTest(
                    "unit argument",
                    "{fun (x : {}) => 100}({})",
                    "100"
                ),
                BasicTest(
                    "single integer argument",
                    """{fun (x : Int) => x}(10)""",
                    "10"
                ),
                BasicTest(
                    "multiple arguments - first",
                    """{fun (x : Int, y : String) => x}(10, "hello")""",
                    "10"
                ),
                BasicTest(
                    "multiple arguments - second",
                    """{fun (x : Int, y : String) => y}(10, "hello")""",
                    """"hello""""
                ),
                BasicTest(
                    "multi-line function",
                    """{fun (x : Int) =>
                      x}(10)""",
                    "10"
                ),
                BasicTest(
                    "row argument",
                    "{fun (r : {x : Int}) => r.x}({x = 20})",
                    "20"
                ),
                BasicTest(
                    "single field row return",
                    "{fun (x : Int) => {a = x}}(9)",
                    "{a = 9}"
                ),
                BasicTest(
                    "function argument",
                    """{fun (f : Int => String) => f(10)}(fun (x : Int) => "yes")""",
                    """"yes""""
                ),
                BasicTest(
                    "function return then call",
                    "{fun (x : Int) => {fun (y : Int) => x}}(10)(15)",
                    "10"
                ),
                BasicTest(
                    "function program result",
                    "{fun (f : Int => Int) => f}(fun (x : Int) => x)",
                    "<function>"
                ),

                // Blocks

                BasicTest(
                    "trivial block",
                    "{ 10 }",
                    "10"
                ),
                BasicTest(
                    "val block",
                    """{
                       val x = 10
                       x
                    }""",
                    "10"
                ),
                BasicTest(
                    "val block (inner ref)",
                    """{
                        val x = 10
                        val y = 20
                        y
                    }""",
                    "20"
                ),
                BasicTest(
                    "val block (outer ref)",
                    """{
                        val x = 10
                        val y = 20
                        x
                    }""",
                    "10"
                ),
                BasicTest(
                    "val block with functions",
                    """{
                        val f = fun (x : Int) => x
                        val g = fun (y : Int) => f(y)
                        g(10)
                    }""",
                    "10"
                ),
                BasicTest(
                    "def block (single)",
                    """{
                        def f(x : Int) = x
                        f(10)
                    }""",
                    "10"
                ),
                BasicTest(
                    "def block (multi inner)",
                    """{
                        def f(x : Int) = x
                        def g(y : Int) = f(y)
                        g(10)
                    }""",
                    "10"
                ),
                BasicTest(
                    "def block (multi outer)",
                    """{
                        def f(x : Int) = x
                        def g(y : Int) = f(y)
                        f(10)
                    }""",
                    "10"
                ),
                BasicTest(
                    "block (val and def)",
                    """{
                        val a = 20
                        def f(x : Int) = a
                        f(10)
                    }""",
                    "20"
                ),
                BasicTest(
                    "def redefinition",
                    """{
                        def f(x : Int) = 10
                        val a = 20
                        def f(y : Int) = 30
                        f(0)
                    }""",
                    "30"
                ),
                BasicTest(
                    "nested val block (inner)",
                    """{
                        val a = 10
                        {
                            val b = 20
                            b
                        }
                    }""",
                    "20"
                ),
                BasicTest(
                    "nested val block (outer)",
                    """{
                        val a = 10
                        {
                            val b = 20
                            a
                        }
                    }""",
                    "10"
                ),
                BasicTest(
                    "nested val block (redefinition)",
                    """{
                        val a = 10
                        {
                            val a = 20
                            a
                        }
                    }""",
                    "20"
                ),
                BasicTest(
                    "nested def block (outer)",
                    """{
                        def f(x : Int) = 10
                        {
                            def g(y : Int) = 20
                            f(0)
                        }
                    }""",
                    "10"
                ),
                BasicTest(
                    "nested def block (inner)",
                    """{
                        def f(x : Int) = 10
                        {
                            def g(y : Int) = 20
                            g(0)
                        }
                    }""",
                    "20"
                ),
                BasicTest(
                    "nested def block (redefinition)",
                    """{
                        def f(x : Int) = 10
                        {
                            def f(y : Int) = 20
                            f(0)
                        }
                    }""",
                    "20"
                ),

                // Command-line arguments

                BasicTest(
                    "string command argument",
                    "fun (s : String) => s",
                    """"hello"""",
                    Seq("hello"),
                    Some("<function>")
                ),
                BasicTest(
                    "multiple string command arguments",
                    "fun (s : String, t : String) => t",
                    """"there"""",
                    Seq("hello", "there"),
                    Some("<function>")
                )
            )

        // Compile and run tests

        for (aTest <- basicTests) {
            test(s"${backend.name} run: ${aTest.name}") {
                val result = runString(aTest.name, aTest.program, backend.options, aTest.args)
                result shouldBe ""
            }
            test(s"${backend.name} run: ${aTest.name}: result") {
                val result = runString(aTest.name, aTest.program, Seq("-r") ++ backend.options, aTest.args)
                result shouldBe s"${aTest.expectedCompiledResult}\n"
            }
        }

        {
            val resourcesPath = "src/test/resources"

            filetests(s"${backend.name} file", s"${resourcesPath}/basic", ".cooma", ".out",
                argslist = List(backend.options ++ List("-r")))

            case class OptionTest(
                name : String,
                option : String,
                inputBasename : String,
                expectedExtension : String,
                args : Seq[String] = Seq()
            )

            val optionTests =
                List(
                    OptionTest("Cooma AST print", "-C", "basic/singleArgCall", "coomaAST"),
                    OptionTest("IR print", "-i", "basic/singleArgCall", "IR"),
                    OptionTest("IR AST print", "-I", "basic/singleArgCall", "IRAST"),
                    OptionTest("Cooma AST print", "-C", "basic/multiArgCall", "coomaAST"),
                    OptionTest("IR print", "-i", "basic/multiArgCall", "IR"),
                    OptionTest("IR AST print", "-I", "basic/multiArgCall", "IRAST"),
                    OptionTest("Cooma AST print", "-C", "basic/blockVal", "coomaAST"),
                    OptionTest("IR print", "-i", "basic/blockVal", "IR"),
                    OptionTest("IR AST print", "-I", "basic/blockVal", "IRAST"),
                    OptionTest("Cooma AST print", "-C", "basic/blockDef", "coomaAST"),
                    OptionTest("IR print", "-i", "basic/blockDef", "IR"),
                    OptionTest("IR AST print", "-I", "basic/blockDef", "IRAST"),
                    OptionTest("Cooma AST print", "-C", "capability/consoleCmdArg", "coomaAST", Seq("/dev/null")),
                    OptionTest("IR print", "-i", "capability/consoleCmdArg", "IR", Seq("/dev/null")),
                    OptionTest("IR AST print", "-I", "capability/consoleCmdArg", "IRAST", Seq("/dev/null"))
                )

            for (aTest <- optionTests) {
                val inputFilename = s"${aTest.inputBasename}.cooma"
                val expectedFilename = s"${aTest.inputBasename}.${aTest.expectedExtension}"
                val execute = !aTest.name.startsWith("IR") || backend.name == "Reference"
                if (execute)
                    filetest(s"${backend.name} file", resourcesPath, s"$resourcesPath/$expectedFilename",
                        backend.options ++ List(aTest.option, s"$resourcesPath/$inputFilename") ++ aTest.args,
                        expectedFilename)
            }
        }

        // REPL tests

        for (aTest <- basicTests) {
            test(s"${backend.name} REPL: ${aTest.name}") {
                val result = runREPLOnLine(aTest.name, aTest.program, backend.options, aTest.args)
                val expectedResult =
                    aTest.expectedREPLResult match {
                        case Some(s) => s
                        case _       => aTest.expectedCompiledResult
                    }
                result shouldBe s"res0 = $expectedResult\n"
            }
        }

        case class REPLTest(
            name : String,
            program : String,
            expectedResult : String
        )

        val replTests =
            List(
                REPLTest(
                    "single evaluation (int)",
                    """
                        10
                    """,
                    "res0 = 10"
                ),
                REPLTest(
                    "single evaluation (string)",
                    """
                        "Hello"
                    """,
                    """res0 = "Hello""""
                ),
                REPLTest(
                    "multiple evaluations",
                    """
                        10
                        20
                    """,
                    "res0 = 10\nres1 = 20"
                ),
                REPLTest(
                    "single value definition",
                    """
                        val x = 1
                    x
                    """,
                    "x = 1\nres0 = 1"
                ),
                REPLTest(
                    "multiple value definitions (upper)",
                    """
                        val x = 1
                        val y = 2
                        x
                    """,
                    "x = 1\ny = 2\nres0 = 1"
                ),
                REPLTest(
                    "multiple value definitions (lower)",
                    """
                        val x = 1
                        val y = 2
                        y
                    """,
                    "x = 1\ny = 2\nres0 = 2"
                ),
                REPLTest(
                    "single function definition",
                    """
                        def f(x : Int) = x
                        f(10)
                    """,
                    "f\nres0 = 10"
                ),
                REPLTest(
                    "value and function definition",
                    """
                        val x = 10
                        def f(y : Int) = x
                        f(20)
                    """,
                    "x = 10\nf\nres0 = 10"
                ),
                REPLTest(
                    "multiple function definitions (upper)",
                    """
                        def f(x : Int) = 10
                        def g(y : Int) = 20
                        f(1)
                    """,
                    "f\ng\nres0 = 10"
                ),
                REPLTest(
                    "multiple function definitions (lower)",
                    """
                        def f(x : Int) = 10
                        def g(y : Int) = 20
                        g(1)
                    """,
                    "f\ng\nres0 = 20"
                ),
                REPLTest(
                    "multiple function definitions (chain)",
                    """
                        def f(x : Int) = 10
                        def g(y : Int) = f(y)
                        g(1)
                    """,
                    "f\ng\nres0 = 10"
                ),
                REPLTest(
                    "single expression",
                    """
                        10
                        res0
                    """,
                    "res0 = 10\nres1 = 10"
                )

            )

        for (aTest <- replTests) {
            test(s"${backend.name} REPL: ${aTest.name}") {
                val result = runREPLOnLines(aTest.name, aTest.program, backend.options, Seq())
                result shouldBe s"${aTest.expectedResult}\n"
            }
        }

        // Command-line tests

        case class CmdLineTest(
            name : String,
            filename : String,
            expectedResult : String,
            args : Seq[String] = Seq(),
            usedArg : Int
        )

        val cmdLineTests =
            List(
                CmdLineTest(
                    "one string command argument",
                    "src/test/resources/capability/stringCmdArg.cooma",
                    "\"hello\"",
                    Seq("hello"),
                    0
                ),
                CmdLineTest(
                    "two string command arguments",
                    "src/test/resources/capability/multiStringCmdArg.cooma",
                    "\"there\"",
                    Seq("hello", "there"),
                    1
                )
            )

        for (aTest <- cmdLineTests) {
            test(s"${backend.name} run: ${aTest.name} (${aTest.filename})") {
                val result = runFile(aTest.filename, backend.options, aTest.args)
                result shouldBe ""
            }
            test(s"${backend.name} run: ${aTest.name} (${aTest.filename}): result") {
                val result = runFile(aTest.filename, backend.options ++ Seq("-r"), aTest.args)
                result shouldBe s"${aTest.expectedResult}\n"
            }
            test(s"${backend.name} run: ${aTest.name} (${aTest.filename}): no args") {
                val result = runFile(aTest.filename, backend.options, Seq())
                result shouldBe s"cooma: command-line argument ${aTest.usedArg} does not exist (arg count = 0)\n"
            }
        }

        {
            val filename = "src/test/resources/capability/consoleInternalArg.cooma"
            val name = s"console internal argument ($filename)"
            val console = "/tmp/coomaTest.txt"
            val content = "Internal!\n"

            test(s"${backend.name} run: $name") {
                createFile(console, "")
                val result = runFile(filename, backend.options, Seq())
                result shouldBe ""
                FileSource(console).content shouldBe content
                deleteFile(console)
            }

            test(s"${backend.name} run: $name: result") {
                createFile(console, "")
                val result = runFile(filename, backend.options ++ Seq("-r"), Seq())
                result shouldBe "{}\n"
                FileSource(console).content shouldBe content
                deleteFile(console)
            }
        }

        {
            val filename = "src/test/resources/capability/consoleInternalArgBad.cooma"
            val name = s"bad console internal argument ($filename)"
            val console = "/does/not/exist"

            test(s"${backend.name} run: $name") {
                val result = runFile(filename, backend.options, Seq())
                result shouldBe s"cooma: Writer capability unavailable: can't write $console\n"
            }
        }

        {
            val filename = "src/test/resources/capability/readerInternalArg.cooma"
            val name = s"reader internal argument ($filename)"
            val reader = "/tmp/coomaTest.txt"
            val content = "Some reading stuff!\n"

            test(s"${backend.name} run: $name") {
                createFile(reader, content)
                val result = runFile(filename, backend.options, Seq())
                result shouldBe ""
                FileSource(reader).content shouldBe content
                deleteFile(reader)
            }
        }

        {
            val filename = "src/test/resources/capability/readerInternalArgBad.cooma"
            val name = s"bad reader internal argument ($filename)"
            val reader = "/does/not/exist.txt"

            test(s"${backend.name} run: $name") {
                val result = runFile(filename, backend.options, Seq())
                result shouldBe s"cooma: Reader capability unavailable: can't read $reader\n"
            }
        }

        {
            val filename = "src/test/resources/capability/consoleCmdArg.cooma"
            val name = s"console command argument ($filename)"
            val console = makeTempFilename(".txt")
            val args = Seq(console)
            val content = "Hello world!\n"

            test(s"${backend.name} run: $name") {
                createFile(console, "")
                val result = runFile(filename, backend.options, args)
                result shouldBe ""
                FileSource(console).content shouldBe content
                deleteFile(console)
            }

            test(s"${backend.name} run: $name: result") {
                createFile(console, "")
                val result = runFile(filename, backend.options ++ Seq("-r"), args)
                result shouldBe "{}\n"
                FileSource(console).content shouldBe content
                deleteFile(console)
            }

            test(s"${backend.name} run: $name: non-existent console") {
                val console = "notThere.txt"
                val result = runFile(filename, backend.options, Seq(console))
                result shouldBe s"cooma: Writer capability unavailable: can't write $console\n"
                Files.exists(Paths.get(console)) shouldBe false
            }

            test(s"${backend.name} run: $name: no args") {
                val result = runFile(filename, backend.options, Seq())
                result shouldBe s"cooma: command-line argument 0 does not exist (arg count = 0)\n"
            }
        }

        {
            val filename = "src/test/resources/capability/consoleReaderCmdArg.cooma"
            val name = s"console and reader command arguments ($filename)"
            val console = makeTempFilename(".txt")
            val reader = makeTempFilename(".txt")
            val args = Seq(console, reader)
            val content = "The file contents"

            test(s"${backend.name} run: $name") {
                createFile(console, "")
                createFile(reader, content)
                val result = runFile(filename, backend.options, args)
                result shouldBe ""
                FileSource(console).content shouldBe content
                FileSource(reader).content shouldBe content
                deleteFile(console)
                deleteFile(reader)
            }

            test(s"${backend.name} run: $name: result") {
                createFile(console, "")
                createFile(reader, content)
                val result = runFile(filename, backend.options ++ Seq("-r"), args)
                result shouldBe "{}\n"
                FileSource(console).content shouldBe content
                FileSource(reader).content shouldBe content
                deleteFile(console)
                deleteFile(reader)
            }

            test(s"${backend.name} run: $name: non-existent console") {
                createFile(reader, "")
                val console = "notThere.txt"
                val result = runFile(filename, backend.options, Seq(console, reader))
                result shouldBe s"cooma: Writer capability unavailable: can't write $console\n"
                Files.exists(Paths.get(console)) shouldBe false
                deleteFile(console)
            }

            test(s"${backend.name} run: $name: non-existent reader") {
                createFile(console, "")
                val reader = "notThere.txt"
                val result = runFile(filename, backend.options, Seq(console, reader))
                result shouldBe s"cooma: Reader capability unavailable: can't read $reader\n"
                Files.exists(Paths.get(reader)) shouldBe false
                deleteFile(console)
            }

            test(s"${backend.name} run: $name: no args") {
                val result = runFile(filename, backend.options, Seq())
                result shouldBe s"cooma: command-line argument 1 does not exist (arg count = 0)\n"
            }

            test(s"${backend.name} run: $name: one arg") {
                createFile(console, "")
                val result = runFile(filename, backend.options, Seq(console))
                result shouldBe s"cooma: command-line argument 1 does not exist (arg count = 1)\n"
                deleteFile(console)
            }
        }

        {
            val filename = "src/test/resources/capability/consoleReaderWriterInternalArg.cooma"
            val name = s"console internal argument reader writer ($filename)"
            val console = "/tmp/coomaTest.txt"
            val content = "Internal reader writer!\n"

            test(s"${backend.name} run: $name") {
                createFile(console, "")
                val result = runFile(filename, backend.options, Seq())
                result shouldBe ""
                FileSource(console).content shouldBe content
                deleteFile(console)
            }

            test(s"${backend.name} run: $name: result") {
                createFile(console, "")
                val result = runFile(filename, backend.options ++ Seq("-r"), Seq())
                result shouldBe "{}\n"
                FileSource(console).content shouldBe content
                deleteFile(console)
            }
        }
    }

    def makeConfig(args : Seq[String]) : Config = {
        // Set Scallop so that errors don't just exit the process
        val saveThrowError = throwError.value
        throwError.value = true
        val config = createConfig(args)
        config.verify()
        throwError.value = saveThrowError
        config
    }

    def runTest(name : String, tester : Config => Unit, options : Seq[String], args : Seq[String]) : String = {
        val config = makeConfig(args)
        try {
            tester(config)
        } catch {
            case e : Exception =>
                info("failed with an exception ")
                throw (e)
        }
        config.stringEmitter.result
    }

    def runString(name : String, program : String, options : Seq[String], args : Seq[String]) : String = {
        val allArgs = Seq("--Koutput", "string") ++ options ++ ("test.cooma" +: args)
        runTest(name, compileString(name, program, _), options, allArgs)
    }

    def runFile(program : String, options : Seq[String], args : Seq[String]) : String = {
        val allArgs = Seq("--Koutput", "string") ++ options ++ (program +: args)
        runTest(name, compileFile(program, _), options, allArgs)
    }

    def runREPLTest(name : String, cmd : String, input : String, options : Seq[String], args : Seq[String]) : String = {
        val allArgs = Seq("--Koutput", "string") ++ options ++ args
        val config = makeConfig(allArgs)
        val replInput =
            if (input.indexOf('\n') == -1)
                input
            else
                s"$cmd\n$input\n:end"
        val console = new StringConsole(replInput)
        val repl = createREPL(config)
        runTest(name, repl.processconsole(console, "dummy", _), options, allArgs)
    }

    def runREPLOnLine(name : String, input : String, options : Seq[String], args : Seq[String]) : String =
        runREPLTest(name, ":paste", input, options, args)

    def runREPLOnLines(name : String, input : String, options : Seq[String], args : Seq[String]) : String =
        runREPLTest(name, ":lines", input, options, args)

}
