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
    import org.bitbucket.inkytonik.kiama.util.FileSource
    import org.bitbucket.inkytonik.kiama.util.IO.{createFile, deleteFile}
    import org.rogach.scallop.throwError

    // Basic tests

    case class Test(
        name : String,
        program : String,
        expectedResult : String,
        args : Seq[String] = Seq()
    )

    val tests =
        List(
            // Primitive values

            Test(
                "integer",
                "42",
                "42"
            ),
            Test(
                "string",
                """"hello"""",
                """"hello""""
            ),
            Test(
                "string with quote",
                """"hel\"lo"""",
                """"hel\"lo""""
            ),
            Test(
                "string with newline",
                """"hello\n"""",
                """"hello\n""""
            ),
            Test(
                "string with escape sequences",
                """"\b\t\n\f\t\7\15\167"""",
                """"\b\t\n\f\t\7\rw""""
            ),

            // Rows

            Test(
                "unit",
                "{}",
                "{}"
            ),
            Test(
                "row (single int field)",
                "{x = 65}",
                "{x = 65}"
            ),
            Test(
                "row (single string field)",
                """{name = "Harold"}""",
                """{name = "Harold"}"""
            ),
            Test(
                "row (two fields)",
                "{a = 1, b = 2}",
                "{a = 1, b = 2}"
            ),
            Test(
                "row (many fields)",
                """{name = "Bob", age = 24, year = 1998, sex = "F"}""",
                """{name = "Bob", age = 24, year = 1998, sex = "F"}"""
            ),
            Test(
                "field select (first of one)",
                """{s = "Hi"}.s""",
                """"Hi""""
            ),
            Test(
                "field select (first of two)",
                """{s = "Hi", t = 10}.s""",
                """"Hi""""
            ),
            Test(
                "field select (second of two)",
                """{s = "Hi", t = 10}.t""",
                "10"
            ),
            Test(
                "field select (many fields)",
                """{name = "Bob", age = 24, year = 1998, sex = "F"}.sex""",
                """"F""""
            ),
            Test(
                "nested field select",
                "{r = {y = 42}}.r.y",
                "42"
            ),
            Test(
                "row concatenation",
                """{ val r = {x = 10, y = 20} val s = {a = "Hi"} r & s }""",
                """{x = 10, y = 20, a = "Hi"}"""
            ),
            Test(
                "select from row concatenation (left)",
                """{ val r = {x = 10, y = 20} val s = {a = "Hi"} {r & s}.x }""",
                "10"
            ),
            Test(
                "select from row concatenation (right)",
                """{ val r = {x = 10, y = 20} val s = {a = "Hi"} {r & s}.a }""",
                """"Hi""""
            ),

            // Functions

            Test(
                "unit argument",
                "{fun (x : {}) => 100}({})",
                "100"
            ),
            Test(
                "single integer argument",
                """{fun (x : Int) => x}(10)""",
                "10"
            ),
            Test(
                "multiple arguments - first",
                """{fun (x : Int, y : String) => x}(10, "hello")""",
                "10"
            ),
            Test(
                "multiple arguments - second",
                """{fun (x : Int, y : String) => y}(10, "hello")""",
                """"hello""""
            ),
            Test(
                "row argument",
                "{fun (r : {x : Int}) => r.x}({x = 20})",
                "20"
            ),
            Test(
                "single field row return",
                "{fun (x : Int) => {a = x}}(9)",
                "{a = 9}"
            ),
            Test(
                "function argument",
                """{fun (f : Int => String) => f(10)}(fun (x : Int) => "yes")""",
                """"yes""""
            ),
            Test(
                "function return then call",
                "{fun (x : Int) => {fun (y : Int) => x}}(10)(15)",
                "10"
            ),
            Test(
                "function program result",
                "{fun (f : Int => Int) => f}(fun (x : Int) => x)",
                "<function>"
            ),

            // Blocks

            Test(
                "trivial block",
                "{ 10 }",
                "10"
            ),
            Test(
                "val block",
                "{ val x = 10 x }",
                "10"
            ),
            Test(
                "val block (inner ref)",
                "{ val x = 10 val y = 20 y }",
                "20"
            ),
            Test(
                "val block (outer ref)",
                "{ val x = 10 val y = 20 x }",
                "10"
            ),
            Test(
                "val block with functions",
                """
                {
                    val f = fun (x : Int) => x
                    val g = fun (y : Int) => f(y)
                    g(10)
                }
                """,
                "10"
            ),
            Test(
                "def block (single)",
                """
                {
                    def f(x : Int) = x
                    f(10)
                }
                """,
                "10"
            ),
            Test(
                "def block (multi inner)",
                """
                {
                    def f(x : Int) = x
                    def g(y : Int) = f(y)
                    g(10)
                }
                """,
                "10"
            ),
            Test(
                "def block (multi outer)",
                """
                {
                    def f(x : Int) = x
                    def g(y : Int) = f(y)
                    f(10)
                }
                """,
                "10"
            ),
            Test(
                "block (val and def)",
                """
                {
                    val a = 20
                    def f(x : Int) = a
                    f(10)
                }
                """,
                "20"
            ),
            Test(
                "def redefinition",
                """
                {
                    def f(x : Int) = 10
                    val a = 20
                    def f(y : Int) = 30
                    f(0)
                }
                """,
                "30"
            ),
            Test(
                "nested val block (inner)",
                """
                {
                    val a = 10
                    {
                        val b = 20
                        b
                    }
                }
                """,
                "20"
            ),
            Test(
                "nested val block (outer)",
                """
                {
                    val a = 10
                    {
                        val b = 20
                        a
                    }
                }
                """,
                "10"
            ),
            Test(
                "nested val block (redefinition)",
                """
                {
                    val a = 10
                    {
                        val a = 20
                        a
                    }
                }
                """,
                "20"
            ),
            Test(
                "nested def block (outer)",
                """
                {
                    def f(x : Int) = 10
                    {
                        def g(y : Int) = 20
                        f(0)
                    }
                }
                """,
                "10"
            ),
            Test(
                "nested def block (inner)",
                """
                {
                    def f(x : Int) = 10
                    {
                        def g(y : Int) = 20
                        g(0)
                    }
                }
                """,
                "20"
            ),
            Test(
                "nested def block (redefinition)",
                """
                {
                    def f(x : Int) = 10
                    {
                        def f(y : Int) = 20
                        f(0)
                    }
                }
                """,
                "20"
            ),

            // Command-line arguments

            Test(
                "string command argument",
                "fun (s : String) => s",
                """"hello"""",
                Seq("hello")
            ),
            Test(
                "multiple string command arguments",
                "fun (s : String, t : String) => t",
                """"there"""",
                Seq("hello", "there")
            )
        )

    for (aTest <- tests) {
        test(aTest.name) {
            val result = runCoomaOnString(aTest.name, aTest.program, Seq(), aTest.args)
            result shouldBe ""
        }
        test(s"${aTest.name}: result") {
            val result = runCoomaOnString(aTest.name, aTest.program, Seq("-r"), aTest.args)
            result shouldBe s"${aTest.expectedResult}\n"
        }
    }

    filetests("file execution: basic", "src/test/resources/basic", ".cooma", ".out",
        argslist = List(List("-r")))

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
        test(s"${aTest.name} (${aTest.filename})") {
            val result = runCoomaOnFile(aTest.filename, Seq(), aTest.args)
            result shouldBe ""
        }
        test(s"${aTest.name} (${aTest.filename}): result") {
            val result = runCoomaOnFile(aTest.filename, Seq("-r"), aTest.args)
            result shouldBe s"${aTest.expectedResult}\n"
        }
        test(s"${aTest.name} (${aTest.filename}): no args") {
            val result = runCoomaOnFile(aTest.filename, Seq(), Seq())
            result shouldBe s"cooma: command-line argument ${aTest.usedArg} does not exist (arg count = 0)\n"
        }
    }

    {
        val filename = "src/test/resources/capability/consoleCmdArg.cooma"
        val name = s"console command argument ($filename)"
        val console = makeTempFilename(".txt")
        val args = Seq(console)
        val content = "Hello world!\n"

        test(name) {
            createFile(console, "")
            val result = runCoomaOnFile(filename, Seq(), args)
            result shouldBe ""
            FileSource(console).content shouldBe content
            deleteFile(console)
        }

        test(s"$name: result") {
            createFile(console, "")
            val result = runCoomaOnFile(filename, Seq("-r"), args)
            result shouldBe "{}\n"
            FileSource(console).content shouldBe content
            deleteFile(console)
        }

        test(s"$name: non-existent console") {
            val console = "notThere.txt"
            val result = runCoomaOnFile(filename, Seq(), Seq(console))
            result shouldBe s"cooma: Console capability unavailable: can't write $console\n"
            Files.exists(Paths.get(console)) shouldBe false
        }

        test(s"$name: no args") {
            val result = runCoomaOnFile(filename, Seq(), Seq())
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

        test(s"$name") {
            createFile(console, "")
            createFile(reader, content)
            val result = runCoomaOnFile(filename, Seq(), args)
            result shouldBe ""
            FileSource(console).content shouldBe content
            FileSource(reader).content shouldBe content
            deleteFile(console)
            deleteFile(reader)
        }

        test(s"$name: result") {
            createFile(console, "")
            createFile(reader, content)
            val result = runCoomaOnFile(filename, Seq("-r"), args)
            result shouldBe "{}\n"
            FileSource(console).content shouldBe content
            FileSource(reader).content shouldBe content
            deleteFile(console)
            deleteFile(reader)
        }

        test(s"$name: non-existent console") {
            createFile(reader, "")
            val console = "notThere.txt"
            val result = runCoomaOnFile(filename, Seq(), Seq(console, reader))
            result shouldBe s"cooma: Console capability unavailable: can't write $console\n"
            Files.exists(Paths.get(console)) shouldBe false
            deleteFile(console)
        }

        test(s"$name: non-existent reader") {
            createFile(console, "")
            val reader = "notThere.txt"
            val result = runCoomaOnFile(filename, Seq(), Seq(console, reader))
            result shouldBe s"cooma: Reader capability unavailable: can't read $reader\n"
            Files.exists(Paths.get(reader)) shouldBe false
            deleteFile(console)
        }

        test(s"$name: no args") {
            val result = runCoomaOnFile(filename, Seq(), Seq())
            result shouldBe s"cooma: command-line argument 1 does not exist (arg count = 0)\n"
        }

        test(s"$name: one arg") {
            createFile(console, "")
            val result = runCoomaOnFile(filename, Seq(), Seq(console))
            result shouldBe s"cooma: command-line argument 1 does not exist (arg count = 1)\n"
            deleteFile(console)
        }
    }

    def makeConfig(args : Seq[String]) : Config = {
        // Set Scallop so that errors don't just exit the process
        val saveThrowError = throwError.value
        throwError.value = true
        val config = Main.createConfig(args)
        config.verify()
        throwError.value = saveThrowError
        config
    }

    def runCoomaOnString(name : String, program : String, options : Seq[String], args : Seq[String]) : String = {
        val allArgs = Seq("--Koutput", "string") ++ options ++ ("test.cooma" +: args)
        val config = makeConfig(allArgs)
        try {
            Main.compileString(name, program, config)
        } catch {
            case e : Exception =>
                info("failed with an exception ")
                throw (e)
        }
        config.stringEmitter.result
    }

    def runCoomaOnFile(program : String, options : Seq[String], args : Seq[String]) : String = {
        val allArgs = Seq("--Koutput", "string") ++ options ++ (program +: args)
        val config = makeConfig(allArgs)
        try {
            Main.compileFile(program, config)
        } catch {
            case e : Exception =>
                info("failed with an exception ")
                throw (e)
        }
        config.stringEmitter.result
    }

}
