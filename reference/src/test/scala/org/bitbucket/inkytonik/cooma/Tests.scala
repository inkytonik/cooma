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

import java.io.{ByteArrayOutputStream, PrintStream}

import org.bitbucket.inkytonik.cooma.CoomaParserSyntax.{ASTNode, Program}
import org.bitbucket.inkytonik.cooma.backend.ReferenceBackend
import org.bitbucket.inkytonik.cooma.truffle.{GraalVMBackend, TruffleDriver, TruffleFrontend, TruffleREPL}
import org.bitbucket.inkytonik.kiama.util.{Source, StringConsole, TestCompilerWithConfig}

class Tests extends Driver with TestCompilerWithConfig[ASTNode, Program, Config] {

    import java.nio.file.{Files, Paths}

    import org.bitbucket.inkytonik.kiama.util.FileSource
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
            Backend("GraalVM", Seq("-g"), new TruffleFrontend(out = new PrintStream(truffleOutContent)))
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
                val result = runString(aTest.name, aTest.program, backend.options, backend, aTest.args)
                result shouldBe ""
            }
            test(s"${backend.name} run: ${aTest.name}: result") {
                val result = runString(aTest.name, aTest.program, Seq("-r") ++ backend.options, backend, aTest.args)
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
            val writerInternalArgTests =
                Vector(
                    "src/test/resources/capability/writerInternalDefArg.cooma",
                    "src/test/resources/capability/writerInternalFunArg.cooma"
                )

            for (filename <- writerInternalArgTests) {
                val name = s"writer internal function argument ($filename)"
                val writer = "/tmp/coomaTest.txt"
                val content = "Internal!\n"

                test(s"${backend.name} run: $name") {
                    createFile(writer, "")
                    val result = runFile(filename, backend.options, backend, Seq())
                    result shouldBe ""
                    FileSource(writer).content shouldBe content
                    deleteFile(writer)
                }

                test(s"${backend.name} run: $name: result") {
                    createFile(writer, "")
                    val result = runFile(filename, backend.options ++ Seq("-r"), backend, Seq())
                    result shouldBe "{}\n"
                    FileSource(writer).content shouldBe content
                    deleteFile(writer)
                }
            }
        }

        {
            val filename = "src/test/resources/capability/writerInternalArgBad.cooma"
            val name = s"bad writer internal argument ($filename)"
            val writer = "/does/not/exist"

            test(s"${backend.name} run: $name") {
                val result = runFile(filename, backend.options, backend, Seq())
                result shouldBe s"cooma: Writer capability unavailable: can't write $writer\n"
            }
        }

        {
            val readerInternalArgTests =
                Vector(
                    "src/test/resources/capability/readerInternalDefArg.cooma",
                    "src/test/resources/capability/readerInternalFunArg.cooma"
                )

            for (filename <- readerInternalArgTests) {
                val name = s"reader internal argument ($filename)"
                val reader = "/tmp/coomaTest.txt"
                val content = "Some reading stuff!\n"

                test(s"${backend.name} run: $name") {
                    createFile(reader, content)
                    val result = runFile(filename, backend.options, backend, Seq())
                    result shouldBe ""
                    FileSource(reader).content shouldBe content
                    deleteFile(reader)
                }
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
            val filename = "src/test/resources/capability/readerInternalArgBad.cooma"
            val name = s"bad reader internal argument ($filename)"
            val reader = "/does/not/exist.txt"

            test(s"${backend.name} run: $name") {
                val result = runFile(filename, backend.options, backend, Seq())
                result shouldBe s"cooma: Reader capability unavailable: can't read $reader\n"
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
            println(s"writer file $writer")
            println(s"reader file $reader")
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

        {
            val filename = "src/test/resources/capability/readerWriterInternalArg.cooma"
            val name = s"internal argument reader writer ($filename)"
            val readerWriter = "/tmp/coomaTest.txt"
            val content = "Internal reader writer!\n"

            test(s"${backend.name} run: $name") {
                createFile(readerWriter, "")
                val result = runFile(filename, backend.options, backend, Seq())
                result shouldBe ""
                FileSource(readerWriter).content shouldBe content
                deleteFile(readerWriter)
            }

            test(s"${backend.name} run: $name: result") {
                createFile(readerWriter, "")
                val result = runFile(filename, backend.options ++ Seq("-r"), backend, Seq())
                result shouldBe "{}\n"
                FileSource(readerWriter).content shouldBe content
                deleteFile(readerWriter)
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

    def runString(name : String, program : String, options : Seq[String], backend : Backend, args : Seq[String]) : String = {
        val allArgs = Seq("--Koutput", "string") ++ options ++ ("test.cooma" +: args)
        runTest(name, backend.frontend.interpret(name, program, _), options, allArgs)
    }

    def runFile(program : String, options : Seq[String], backend : Backend, args : Seq[String]) : String = {
        val allArgs = Seq("--Koutput", "string") ++ options ++ (program +: args)
        runTest(name, backend.frontend.interpret, options, allArgs)
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

    override def createREPL(config : Config) : REPL with Compiler with org.bitbucket.inkytonik.cooma.Backend = {
        if (config.graalVM()) new GraalVMBackend(config) with TruffleREPL with Compiler else new ReferenceBackend(config) with REPL with Compiler
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
