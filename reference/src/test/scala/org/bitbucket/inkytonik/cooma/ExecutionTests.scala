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

class ExecutionTests extends Driver with TestCompilerWithConfig[ASTNode, Program, Config] {

    import java.io.{ByteArrayOutputStream, PrintStream}
    import java.nio.file.{Files, Paths}
    import org.bitbucket.inkytonik.cooma.backend.ReferenceBackend
    import org.bitbucket.inkytonik.cooma.truffle.{TruffleBackend, TruffleDriver, TruffleFrontend, TruffleREPL}
    import org.bitbucket.inkytonik.kiama.util.{FileSource, Source, StringConsole}
    import org.bitbucket.inkytonik.kiama.util.Filenames.makeTempFilename
    import org.bitbucket.inkytonik.kiama.util.IO.{createFile, deleteFile}
    import org.rogach.scallop.throwError

    case class Backend(
        name : String,
        options : Seq[String],
        frontend : Frontend
    )

    val truffleOutContent = new ByteArrayOutputStream()

    val backends =
        List(
            Backend("Reference", Seq(), new ReferenceFrontend),
            Backend(
                "GraalVM", Seq("-g"),
                new TruffleFrontend(out = new PrintStream(truffleOutContent))
            )
        )

    for (backend <- backends) {

        // Basic tests

        case class BasicTest(
            name : String,
            program : String,
            expectedCompiledResult : String,
            expectedREPLType : String
        )

        val basicTests =
            List(
                // Primitive values

                BasicTest(
                    "positive integer",
                    "42",
                    "42",
                    "Int"
                ),
                BasicTest(
                    "bracketed expression",
                    "{10}",
                    "10",
                    "Int"
                ),
                BasicTest(
                    "positive integer larger than 32 bits",
                    "4294967296123",
                    "4294967296123",
                    "Int"
                ),
                BasicTest(
                    "positive integer larger than 64 bits",
                    "123456789123456789123456789123456789",
                    "123456789123456789123456789123456789",
                    "Int"
                ),
                BasicTest(
                    "negative integer",
                    "-182",
                    "-182",
                    "Int"
                ),
                BasicTest(
                    "negative integer larger than 32 bits",
                    "-4294967296123",
                    "-4294967296123",
                    "Int"
                ),
                BasicTest(
                    "negative integer larger than 64 bits",
                    "-123456789123456789123456789123456789",
                    "-123456789123456789123456789123456789",
                    "Int"
                ),
                BasicTest(
                    "string",
                    """"hello"""",
                    """"hello"""",
                    "String"
                ),
                BasicTest(
                    "string with quote",
                    """"hel\"lo"""",
                    """"hel\"lo"""",
                    "String"
                ),
                BasicTest(
                    "string with newline",
                    """"hello\n"""",
                    """"hello\n"""",
                    "String"
                ),
                BasicTest(
                    "string with escape sequences",
                    """"\b\t\n\f\t\7\15\167"""",
                    """"\b\t\n\f\t\7\rw"""",
                    "String"
                ),

                // Rows

                BasicTest(
                    "unit",
                    "{}",
                    "{}",
                    "Unit"
                ),
                BasicTest(
                    "row (single int field)",
                    "{x = 65}",
                    "{x = 65}",
                    "{x : Int}"
                ),
                BasicTest(
                    "row (single string field)",
                    """{name = "Harold"}""",
                    """{name = "Harold"}""",
                    "{name : String}"
                ),
                BasicTest(
                    "row (two fields)",
                    "{a = 1, b = 2}",
                    "{a = 1, b = 2}",
                    "{a : Int, b : Int}"
                ),
                BasicTest(
                    "row (many fields)",
                    """{name = "Bob", age = 24, year = 1998, sex = "F"}""",
                    """{name = "Bob", age = 24, year = 1998, sex = "F"}""",
                    "{name : String, age : Int, year : Int, sex : String}"
                ),
                BasicTest(
                    "multi-line row",
                    """{
                        name = "Bob",
                        age = 24
                    }""",
                    """{name = "Bob", age = 24}""",
                    "{name : String, age : Int}"
                ),
                BasicTest(
                    "field select (first of one)",
                    """{s = "Hi"}.s""",
                    """"Hi"""",
                    "String"
                ),
                BasicTest(
                    "field select (first of two)",
                    """{s = "Hi", t = 10}.s""",
                    """"Hi"""",
                    "String"
                ),
                BasicTest(
                    "field select (second of two)",
                    """{s = "Hi", t = 10}.t""",
                    "10",
                    "Int"
                ),
                BasicTest(
                    "field select (many fields)",
                    """{name = "Bob", age = 24, year = 1998, sex = "F"}.sex""",
                    """"F"""",
                    "String"
                ),
                BasicTest(
                    "nested field select",
                    "{r = {y = 42}}.r.y",
                    "42",
                    "Int"
                ),
                BasicTest(
                    "row concatenation",
                    """{
                        val r = {x = 10, y = 20}
                        val s = {a = "Hi"}
                        r & s
                    }""",
                    """{x = 10, y = 20, a = "Hi"}""",
                    "{x : Int, y : Int, a : String}"
                ),
                BasicTest(
                    "select from row concatenation (left)",
                    """{
                       val r = {x = 10, y = 20}
                        val s = {a = "Hi"}
                       {r & s}.x
                   }""",
                    "10",
                    "Int"
                ),
                BasicTest(
                    "select from row concatenation (right)",
                    """{
                       val r = {x = 10, y = 20}
                       val s = {a = "Hi"}
                        {r & s}.a
                   }""",
                    """"Hi"""",
                    "String"
                ),

                // Functions

                BasicTest(
                    "no arguments",
                    "{fun () = 100}()",
                    "100",
                    "Int"
                ),
                BasicTest(
                    "unit argument",
                    "{fun (x : Unit) = 100}({})",
                    "100",
                    "Int"
                ),
                BasicTest(
                    "single integer argument",
                    """{fun (x : Int) = x}(10)""",
                    "10",
                    "Int"
                ),
                BasicTest(
                    "multiple arguments - first",
                    """{fun (x : Int, y : String) = x}(10, "hello")""",
                    "10",
                    "Int"
                ),
                BasicTest(
                    "multiple arguments - second",
                    """{fun (x : Int, y : String) = y}(10, "hello")""",
                    """"hello"""",
                    "String"
                ),
                BasicTest(
                    "multi-line function",
                    """{fun (x : Int) =
                      x}(10)""",
                    "10",
                    "Int"
                ),
                BasicTest(
                    "row argument",
                    "{fun (r : {x : Int}) = r.x}({x = 20})",
                    "20",
                    "Int"
                ),
                BasicTest(
                    "single field row return",
                    "{fun (x : Int) = {a = x}}(9)",
                    "{a = 9}",
                    "{a : Int}"
                ),
                BasicTest(
                    "function argument",
                    """{fun (f : (Int) => String) = f(10)}(fun (x : Int) = "yes")""",
                    """"yes"""",
                    "String"
                ),
                BasicTest(
                    "function return then call",
                    "{fun (x : Int) = fun (y : Int) = x}(10)(15)",
                    "10",
                    "Int"
                ),
                BasicTest(
                    "function program result",
                    "{fun (f : (Int) => Int) = f}(fun (x : Int) = x)",
                    "<function>",
                    "(Int) => Int"
                ),

                // Blocks

                BasicTest(
                    "trivial block",
                    "{ 10 }",
                    "10",
                    "Int"
                ),
                BasicTest(
                    "val block",
                    """{
                       val x = 10
                       x
                    }""",
                    "10",
                    "Int"
                ),
                BasicTest(
                    "val block (inner ref)",
                    """{
                        val x = 10
                        val y = 20
                        y
                    }""",
                    "20",
                    "Int"
                ),
                BasicTest(
                    "val block (outer ref)",
                    """{
                        val x = 10
                        val y = 20
                        x
                    }""",
                    "10",
                    "Int"
                ),
                BasicTest(
                    "val block with functions",
                    """{
                        val f = fun (x : Int) = x
                        val g = fun (y : Int) = f(y)
                        g(10)
                    }""",
                    "10",
                    "Int"
                ),
                BasicTest(
                    "def block no arguments",
                    """{
                        def f() : Int = 10
                        f()
                    }""",
                    "10",
                    "Int"
                ),
                BasicTest(
                    "def block (single)",
                    """{
                        def f(x : Int) : Int = x
                        f(10)
                    }""",
                    "10",
                    "Int"
                ),
                BasicTest(
                    "def block (multi inner)",
                    """{
                        def f(x : Int) : Int = x
                        def g(y : Int) : Int = f(y)
                        g(10)
                    }""",
                    "10",
                    "Int"
                ),
                BasicTest(
                    "def block (multi outer)",
                    """{
                        def f(x : Int) : Int = x
                        def g(y : Int) : Int = f(y)
                        f(10)
                    }""",
                    "10",
                    "Int"
                ),
                BasicTest(
                    "block (val and def)",
                    """{
                        val a = 20
                        def f(x : Int) : Int = a
                        f(10)
                    }""",
                    "20",
                    "Int"
                ),
                BasicTest(
                    "def redefinition",
                    """{
                        def f(x : Int) : Int = 10
                        val a = 20
                        def f(y : Int) : Int = 30
                        f(0)
                    }""",
                    "30",
                    "Int"
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
                    "20",
                    "Int"
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
                    "10",
                    "Int"
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
                    "20",
                    "Int"
                ),
                BasicTest(
                    "nested def block (outer)",
                    """{
                        def f(x : Int) : Int = 10
                        {
                            def g(y : Int) : Int = 20
                            f(0)
                        }
                    }""",
                    "10",
                    "Int"
                ),
                BasicTest(
                    "nested def block (inner)",
                    """{
                        def f(x : Int) : Int = 10
                        {
                            def g(y : Int) : Int = 20
                            g(0)
                        }
                    }""",
                    "20",
                    "Int"
                ),
                BasicTest(
                    "nested def block (redefinition)",
                    """{
                        def f(x : Int) : Int = 10
                        {
                            def f(y : Int) : Int = 20
                            f(0)
                        }
                    }""",
                    "20",
                    "Int"
                )
            )

        // Compile and run tests

        for (aTest <- basicTests) {
            test(s"${backend.name} run: ${aTest.name}") {
                val result = runString(aTest.name, aTest.program, backend.options, backend)
                result shouldBe ""
            }
            test(s"${backend.name} run: ${aTest.name}: result") {
                val result = runString(aTest.name, aTest.program, Seq("-r") ++ backend.options, backend)
                result shouldBe s"${aTest.expectedCompiledResult}\n"
            }
        }

        {
            val resourcesPath = "src/test/resources"
            filetests(s"${backend.name} file", s"${resourcesPath}/basic", ".cooma", ".out",
                argslist = List(backend.options ++ List("-r")))

            filetests(s"${backend.name} file", s"${resourcesPath}/bad", ".cooma", ".out",
                argslist = List(backend.options))

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
                    OptionTest("Cooma AST print", "-C", "capability/writerCmdArg", "coomaAST", Seq("/dev/null")),
                    OptionTest("IR print", "-i", "capability/writerCmdArg", "IR", Seq("/dev/null")),
                    OptionTest("IR AST print", "-I", "capability/writerCmdArg", "IRAST", Seq("/dev/null"))
                )

            for (aTest <- optionTests) {
                val inputFilename = s"${aTest.inputBasename}.cooma"
                val expectedFilename = s"${aTest.inputBasename}.${aTest.expectedExtension}"
                val execute = backend.name == "Reference"
                if (execute)
                    filetest(s"${backend.name} file", resourcesPath, s"$resourcesPath/$expectedFilename",
                        backend.options ++ List(aTest.option, s"$resourcesPath/$inputFilename") ++ aTest.args,
                        expectedFilename)
            }
        }

        // REPL tests

        for (aTest <- basicTests) {
            test(s"${backend.name} REPL: ${aTest.name}") {
                val result = runREPLOnLine(aTest.name, aTest.program, backend.options)
                val expectedResult = s"res0 : ${aTest.expectedREPLType} = ${aTest.expectedCompiledResult}\n"
                result shouldBe expectedResult
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
                    "res0 : Int = 10"
                ),
                REPLTest(
                    "single evaluation (string)",
                    """
                        "Hello"
                    """,
                    """res0 : String = "Hello""""
                ),
                REPLTest(
                    "multiple evaluations",
                    """
                        10
                        20
                    """,
                    "res0 : Int = 10\nres1 : Int = 20"
                ),
                REPLTest(
                    "single value definition",
                    """
                        val x = 1
                    x
                    """,
                    "x : Int = 1\nres0 : Int = 1"
                ),
                REPLTest(
                    "multiple value definitions (upper)",
                    """
                        val x = 1
                        val y = 2
                        x
                    """,
                    "x : Int = 1\ny : Int = 2\nres0 : Int = 1"
                ),
                REPLTest(
                    "multiple value definitions (lower)",
                    """
                        val x = 1
                        val y = 2
                        y
                    """,
                    "x : Int = 1\ny : Int = 2\nres0 : Int = 2"
                ),
                REPLTest(
                    "single function definition",
                    """
                        def f(x : Int) : Int = x
                        f(10)
                    """,
                    "f : (Int) => Int = <function>\nres0 : Int = 10"
                ),
                REPLTest(
                    "value and function definition",
                    """
                        val x = 10
                        def f(y : Int) : Int = x
                        f(20)
                    """,
                    "x : Int = 10\nf : (Int) => Int = <function>\nres0 : Int = 10"
                ),
                REPLTest(
                    "multiple function definitions (upper)",
                    """
                        def f(x : Int) : Int = 10
                        def g(y : Int) : Int = 20
                        f(1)
                    """,
                    "f : (Int) => Int = <function>\ng : (Int) => Int = <function>\nres0 : Int = 10"
                ),
                REPLTest(
                    "multiple function definitions (lower)",
                    """
                        def f(x : Int) : Int = 10
                        def g(y : Int) : Int = 20
                        g(1)
                    """,
                    "f : (Int) => Int = <function>\ng : (Int) => Int = <function>\nres0 : Int = 20"
                ),
                REPLTest(
                    "multiple function definitions (chain)",
                    """
                        def f(x : Int) : Int = 10
                        def g(y : Int) : Int = f(y)
                        g(1)
                    """,
                    "f : (Int) => Int = <function>\ng : (Int) => Int = <function>\nres0 : Int = 10"
                ),
                REPLTest(
                    "single result name binding",
                    """
                        10
                        res0
                    """,
                    "res0 : Int = 10\nres1 : Int = 10"
                ),
                REPLTest(
                    "multiple result name binding",
                    """
                        10
                        20
                        30
                        res2
                    """,
                    "res0 : Int = 10\nres1 : Int = 20\nres2 : Int = 30\nres3 : Int = 30"
                )

            )

        for (aTest <- replTests) {
            test(s"${backend.name} REPL: ${aTest.name}") {
                val result = runREPLOnLines(aTest.name, aTest.program, backend.options)
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
                val result = runFile(aTest.filename, backend.options, backend, aTest.args)
                result shouldBe ""
            }
            test(s"${backend.name} run: ${aTest.name} (${aTest.filename}): result") {
                val result = runFile(aTest.filename, backend.options ++ Seq("-r"), backend, aTest.args)
                result shouldBe s"${aTest.expectedResult}\n"
            }
            test(s"${backend.name} run: ${aTest.name} (${aTest.filename}): no args") {
                val result = runFile(aTest.filename, backend.options, backend, Seq())
                result shouldBe s"cooma: command-line argument ${aTest.usedArg} does not exist (arg count = 0)\n"
            }
        }

        {
            val filename = "src/test/resources/capability/readerCmdArg.cooma"
            val name = s"reader external argument ($filename)"
            val reader = makeTempFilename(".txt")
            val content = "Contents to be read\n"
            val expectedResult = "\"" + s"${Util.escape(content)}" + "\"" + "\n"
            val args = Seq(reader)

            test(s"${backend.name} run: $name") {
                createFile(reader, content)
                val result = runFile(filename, backend.options, backend, args)
                result shouldBe ""
                deleteFile(reader)
            }

            test(s"${backend.name} run: $name: result") {
                createFile(reader, content)
                val result = runFile(filename, backend.options ++ Seq("-r"), backend, args)
                result shouldBe expectedResult
                deleteFile(reader)
            }
        }

        {
            val filename = "src/test/resources/capability/writerCmdArg.cooma"
            val name = s"writer command argument ($filename)"
            val writer = makeTempFilename(".txt")
            val args = Seq(writer)
            val content = "Hello world!\n"

            test(s"${backend.name} run: $name") {
                createFile(writer, "")
                val result = runFile(filename, backend.options, backend, args)
                result shouldBe ""
                FileSource(writer).content shouldBe content
                deleteFile(writer)
            }

            test(s"${backend.name} run: $name: result") {
                createFile(writer, "")
                val result = runFile(filename, backend.options ++ Seq("-r"), backend, args)
                result shouldBe "{}\n"
                FileSource(writer).content shouldBe content
                deleteFile(writer)
            }

            test(s"${backend.name} run: $name: non-existent writer") {
                val writer = "notThere.txt"
                val result = runFile(filename, backend.options, backend, Seq(writer))
                result shouldBe s"cooma: Writer capability unavailable: can't write $writer\n"
                Files.exists(Paths.get(writer)) shouldBe false
            }

            test(s"${backend.name} run: $name: no args") {
                val result = runFile(filename, backend.options, backend, Seq())
                result shouldBe s"cooma: command-line argument 0 does not exist (arg count = 0)\n"
            }

            test(s"${backend.name} run: $name: standard out") {
                val result = runFile(filename, backend.options, backend, Seq("-"))
                result shouldBe content
            }

        }

        {
            val filename = "src/test/resources/capability/writerAndReaderCmdArg.cooma"
            val name = s"writer and reader command arguments ($filename)"
            val writer = makeTempFilename(".txt")
            val reader = makeTempFilename(".txt")
            val args = Seq(writer, reader)
            val content = "The file contents"

            test(s"${backend.name} run: $name") {
                createFile(writer, "")
                createFile(reader, content)
                val result = runFile(filename, backend.options, backend, args)
                result shouldBe ""
                FileSource(writer).content shouldBe content
                FileSource(reader).content shouldBe content
                deleteFile(writer)
                deleteFile(reader)
            }

            test(s"${backend.name} run: $name: result") {
                createFile(writer, "")
                createFile(reader, content)
                val result = runFile(filename, backend.options ++ Seq("-r"), backend, args)
                result shouldBe "{}\n"
                FileSource(writer).content shouldBe content
                FileSource(reader).content shouldBe content
                deleteFile(writer)
                deleteFile(reader)
            }

            test(s"${backend.name} run: $name: non-existent writer") {
                createFile(reader, "")
                val writer = "notThere.txt"
                val result = runFile(filename, backend.options, backend, Seq(writer, reader))
                result shouldBe s"cooma: Writer capability unavailable: can't write $writer\n"
                Files.exists(Paths.get(writer)) shouldBe false
                deleteFile(writer)
            }

            test(s"${backend.name} run: $name: non-existent reader") {
                createFile(writer, "")
                val reader = "notThere.txt"
                val result = runFile(filename, backend.options, backend, Seq(writer, reader))
                result shouldBe s"cooma: Reader capability unavailable: can't read $reader\n"
                Files.exists(Paths.get(reader)) shouldBe false
                deleteFile(writer)
            }

            test(s"${backend.name} run: $name: no args") {
                val result = runFile(filename, backend.options, backend, Seq())
                result shouldBe s"cooma: command-line argument 1 does not exist (arg count = 0)\n"
            }

            test(s"${backend.name} run: $name: one arg") {
                createFile(writer, "")
                val result = runFile(filename, backend.options, backend, Seq(writer))
                result shouldBe s"cooma: command-line argument 1 does not exist (arg count = 1)\n"
                deleteFile(writer)
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

        if (config.graalVM() && truffleOutContent.size() > 0) {
            val result = truffleOutContent.toString
            truffleOutContent.reset()
            result
        } else {
            config.stringEmitter.result
        }
    }

    def runString(name : String, program : String, options : Seq[String], backend : Backend) : String = {
        val allArgs = Seq("--Koutput", "string") ++ options :+ "test.cooma"
        runTest(name, backend.frontend.interpret(name, program, _), options, allArgs)
    }

    def runFile(program : String, options : Seq[String], backend : Backend, args : Seq[String]) : String = {
        val allArgs = Seq("--Koutput", "string") ++ options ++ (program +: args)
        runTest(name, backend.frontend.interpret, options, allArgs)
    }

    def runREPLTest(name : String, cmd : String, input : String, options : Seq[String]) : String = {
        val allArgs = Seq("--Koutput", "string") ++ options
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

    def runREPLOnLine(name : String, input : String, options : Seq[String]) : String =
        runREPLTest(name, ":paste", input, options)

    def runREPLOnLines(name : String, input : String, options : Seq[String]) : String =
        runREPLTest(name, ":lines", input, options)

    override def createREPL(config : Config) : REPL with Compiler with org.bitbucket.inkytonik.cooma.Backend = {
        if (config.graalVM())
            new TruffleBackend(config) with TruffleREPL with Compiler
        else
            new ReferenceBackend(config) with REPL with Compiler
    }

    /**
     *
     * @param source The original cooma Source
     * @param prog   The cooma source AST.
     * @param config
     */
    override def process(source : Source, prog : Program, config : Config) : Unit = {
        val frontend = if (config.graalVM()) new TruffleDriver else new ReferenceDriver
        frontend.process(source, prog, config)
    }

    override def testdriver(config : Config) : Unit = {
        if (config.graalVM()) new TruffleFrontend().interpret(config) else super.testdriver(config)
    }
}
