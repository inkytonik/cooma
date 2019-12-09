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
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class ExecutionTests extends Driver with TestCompilerWithConfig[ASTNode, Program, Config]
    with ScalaCheckDrivenPropertyChecks {

    import java.io.{ByteArrayOutputStream, PrintStream}
    import java.io.{ByteArrayOutputStream}
    import java.nio.file.{Files, Paths}
    import org.bitbucket.inkytonik.cooma.backend.ReferenceBackend
    import org.bitbucket.inkytonik.cooma.Primitives._
    import org.bitbucket.inkytonik.cooma.truffle.{TruffleBackend, TruffleDriver, TruffleFrontend, TruffleREPL}
    import org.bitbucket.inkytonik.kiama.util.{FileSource, Source, StringConsole, StringSource}
    import org.bitbucket.inkytonik.kiama.util.Filenames.makeTempFilename
    import org.bitbucket.inkytonik.kiama.util.IO.{createFile, deleteFile}
    import org.rogach.scallop.throwError
    import wolfendale.scalacheck.regexp.RegexpGen
    import org.scalacheck.Gen

    case class Backend(
        name : String,
        options : Seq[String],
        frontend : Frontend
    )

    val truffleOutContent = new ByteArrayOutputStream()

    val backends =
        List(
            Backend(
                "Reference", Seq(),
                new ReferenceFrontend
            ),
            Backend(
                "GraalVM", Seq("-g"),
                new TruffleFrontend(out = new PrintStream(truffleOutContent))
            )
        )

    for (backend <- backends) {

        // Basic tests

        case class ExecTest(
            name : String,
            program : String,
            expectedCompiledResult : String,
            expectedREPLType : String,
            expectedREPLVar : String = "res0"
        )

        val ExecTests =
            Vector(
                // Primitive values

                ExecTest(
                    "positive integer",
                    "42",
                    "42",
                    "Int"
                ),
                ExecTest(
                    "bracketed expression",
                    "{10}",
                    "10",
                    "Int"
                ),
                ExecTest(
                    "positive integer larger than 32 bits",
                    "4294967296123",
                    "4294967296123",
                    "Int"
                ),
                ExecTest(
                    "positive integer larger than 64 bits",
                    "123456789123456789123456789123456789",
                    "123456789123456789123456789123456789",
                    "Int"
                ),
                ExecTest(
                    "negative integer",
                    "-182",
                    "-182",
                    "Int"
                ),
                ExecTest(
                    "negative integer larger than 32 bits",
                    "-4294967296123",
                    "-4294967296123",
                    "Int"
                ),
                ExecTest(
                    "negative integer larger than 64 bits",
                    "-123456789123456789123456789123456789",
                    "-123456789123456789123456789123456789",
                    "Int"
                ),
                ExecTest(
                    "string",
                    """"hello"""",
                    """"hello"""",
                    "String"
                ),
                ExecTest(
                    "string with quote",
                    """"hel\"lo"""",
                    """"hel\"lo"""",
                    "String"
                ),
                ExecTest(
                    "string with newline",
                    """"hello\n"""",
                    """"hello\n"""",
                    "String"
                ),
                ExecTest(
                    "string with escape sequences",
                    """"\b\f\n\r\t\\\"\'"""",
                    """"\b\f\n\r\t\\\"\'"""",
                    "String"
                ),

                // Records

                ExecTest(
                    "unit",
                    "{}",
                    "{}",
                    "Unit"
                ),
                ExecTest(
                    "record (single int field)",
                    "{x = 65}",
                    "{x = 65}",
                    "{x : Int}"
                ),
                ExecTest(
                    "record (single string field)",
                    """{name = "Harold"}""",
                    """{name = "Harold"}""",
                    "{name : String}"
                ),
                ExecTest(
                    "record (two fields)",
                    "{a = 1, b = 2}",
                    "{a = 1, b = 2}",
                    "{a : Int, b : Int}"
                ),
                ExecTest(
                    "record (many fields)",
                    """{name = "Bob", age = 24, year = 1998, sex = "F"}""",
                    """{name = "Bob", age = 24, year = 1998, sex = "F"}""",
                    "{name : String, age : Int, year : Int, sex : String}"
                ),
                ExecTest(
                    "record (eval field)",
                    "{a = {fun (x : Int) x}(3), b = 2}",
                    "{a = 3, b = 2}",
                    "{a : Int, b : Int}"
                ),
                ExecTest(
                    "multi-line record",
                    """{
                        name = "Bob",
                        age = 24
                    }""",
                    """{name = "Bob", age = 24}""",
                    "{name : String, age : Int}"
                ),
                ExecTest(
                    "field select (first of one)",
                    """{s = "Hi"}.s""",
                    """"Hi"""",
                    "String"
                ),
                ExecTest(
                    "field select (first of two)",
                    """{s = "Hi", t = 10}.s""",
                    """"Hi"""",
                    "String"
                ),
                ExecTest(
                    "field select (second of two)",
                    """{s = "Hi", t = 10}.t""",
                    "10",
                    "Int"
                ),
                ExecTest(
                    "field select (many fields)",
                    """{name = "Bob", age = 24, year = 1998, sex = "F"}.sex""",
                    """"F"""",
                    "String"
                ),
                ExecTest(
                    "nested field select",
                    "{r = {y = 42}}.r.y",
                    "42",
                    "Int"
                ),
                ExecTest(
                    "record concatenation",
                    """{
                        val r = {x = 10, y = 20}
                        val s = {a = "Hi"}
                        r & s
                    }""",
                    """{x = 10, y = 20, a = "Hi"}""",
                    "{x : Int, y : Int, a : String}"
                ),
                ExecTest(
                    "select from record concatenation (left)",
                    """{
                       val r = {x = 10, y = 20}
                        val s = {a = "Hi"}
                       {r & s}.x
                   }""",
                    "10",
                    "Int"
                ),
                ExecTest(
                    "select from record concatenation (right)",
                    """{
                       val r = {x = 10, y = 20}
                       val s = {a = "Hi"}
                        {r & s}.a
                   }""",
                    """"Hi"""",
                    "String"
                ),

                // Variants

                ExecTest(
                    "variant (single int field)",
                    "<x = 65>",
                    "<x = 65>",
                    "<x : Int>"
                ),
                ExecTest(
                    "variant (single string field)",
                    """<name = "Harold">""",
                    """<name = "Harold">""",
                    "<name : String>"
                ),
                ExecTest(
                    "variant (eval field)",
                    "<a = {fun (x : Int) x}(3)>",
                    "<a = 3>",
                    "<a : Int>"
                ),
                ExecTest(
                    "multi-line variant",
                    """<
                        name = "Bob"
                    >""",
                    """<name = "Bob">""",
                    "<name : String>"
                ),
                ExecTest(
                    "basic match",
                    "<x = 1> match { case x a => a }",
                    "1",
                    "Int"
                ),
                ExecTest(
                    "multi-case match (first case, same order)",
                    """{
                        def f () <x : Int, y : Int> = <x = 3>
                        f () match { case x a => 1 case y b => 2 }
                    }""",
                    "1",
                    "Int"
                ),
                ExecTest(
                    "multi-case match (later case, same order)",
                    """{
                        def f () <x : Int, y : Int> = <y = 3>
                        f () match { case x a => 1 case y b => 2 }
                    }""",
                    "2",
                    "Int"
                ),
                ExecTest(
                    "multi-case match (first case, different order)",
                    """{
                        def f () <x : Int, y : Int> = <y = 3>
                        f () match { case y b => 1 case x a => 2 }
                    }""",
                    "1",
                    "Int"
                ),
                ExecTest(
                    "multi-case match (later case, different order)",
                    """{
                        def f () <x : Int, y : Int> = <x = 3>
                        f () match { case y b => 1 case x a => 2 }
                    }""",
                    "2",
                    "Int"
                ),
                ExecTest(
                    "Boolean match",
                    """{
                        def f (b : Boolean) Unit =
                            b match {
                                case false x => x
                                case true x => x
                            }
                        {a = f(<false = {}>), b = f(<true = {}>)}
                    }""",
                    "{a = {}, b = {}}",
                    "{a : Unit, b : Unit}"
                ),

                // Functions

                ExecTest(
                    "no arguments",
                    "{fun () 100}()",
                    "100",
                    "Int"
                ),
                ExecTest(
                    "unit argument",
                    "{fun (x : Unit) 100}({})",
                    "100",
                    "Int"
                ),
                ExecTest(
                    "single integer argument",
                    """{fun (x : Int) x}(10)""",
                    "10",
                    "Int"
                ),
                ExecTest(
                    "multiple arguments - first",
                    """{fun (x : Int, y : String) x}(10, "hello")""",
                    "10",
                    "Int"
                ),
                ExecTest(
                    "multiple arguments - second",
                    """{fun (x : Int, y : String) y}(10, "hello")""",
                    """"hello"""",
                    "String"
                ),
                ExecTest(
                    "multi-line function",
                    """{fun (x : Int)
                      x}(10)""",
                    "10",
                    "Int"
                ),
                ExecTest(
                    "record argument",
                    "{fun (r : {x : Int}) r.x}({x = 20})",
                    "20",
                    "Int"
                ),
                ExecTest(
                    "single field record return",
                    "{fun (x : Int) {a = x}}(9)",
                    "{a = 9}",
                    "{a : Int}"
                ),
                ExecTest(
                    "variant argument (one)",
                    "{fun (r : <x : Int>) r}(<x = 20>)",
                    "<x = 20>",
                    "<x : Int>"
                ),
                ExecTest(
                    "function argument",
                    """{fun (f : (Int) String) f(10)}(fun (x : Int) "yes")""",
                    """"yes"""",
                    "String"
                ),
                ExecTest(
                    "function return then call",
                    "{fun (x : Int) fun (y : Int) x}(10)(15)",
                    "10",
                    "Int"
                ),
                ExecTest(
                    "function program result",
                    "{fun (f : (Int) Int) f}(fun (x : Int) x)",
                    "<function>",
                    "(Int) Int"
                ),

                // Blocks

                ExecTest(
                    "trivial block",
                    "{ 10 }",
                    "10",
                    "Int"
                ),
                ExecTest(
                    "val block",
                    """{
                       val x = 10
                       x
                    }""",
                    "10",
                    "Int"
                ),
                ExecTest(
                    "val block with redefinition",
                    """{
                       val x = 10
                       val x = 20
                       x
                    }""",
                    "20",
                    "Int"
                ),
                ExecTest(
                    "val block (inner ref)",
                    """{
                        val x = 10
                        val y = 20
                        y
                    }""",
                    "20",
                    "Int"
                ),
                ExecTest(
                    "val block (outer ref)",
                    """{
                        val x = 10
                        val y = 20
                        x
                    }""",
                    "10",
                    "Int"
                ),
                ExecTest(
                    "val block with functions",
                    """{
                        val f = fun (x : Int) x
                        val g = fun (y : Int) f(y)
                        g(10)
                    }""",
                    "10",
                    "Int"
                ),
                ExecTest(
                    "def block no arguments",
                    """{
                        def f() Int = 10
                        f()
                    }""",
                    "10",
                    "Int"
                ),
                ExecTest(
                    "def block (single)",
                    """{
                        def f(x : Int) Int = x
                        f(10)
                    }""",
                    "10",
                    "Int"
                ),
                ExecTest(
                    "def block (multi inner)",
                    """{
                        def f(x : Int) Int = x
                        def g(y : Int) Int = f(y)
                        g(10)
                    }""",
                    "10",
                    "Int"
                ),
                ExecTest(
                    "def block (multi outer)",
                    """{
                        def f(x : Int) Int = x
                        def g(y : Int) Int = f(y)
                        f(10)
                    }""",
                    "10",
                    "Int"
                ),
                ExecTest(
                    "block (val and def)",
                    """{
                        val a = 20
                        def f(x : Int) Int = a
                        f(10)
                    }""",
                    "20",
                    "Int"
                ),
                ExecTest(
                    "def redefinition",
                    """{
                        def f(x : Int) Int = 10
                        val a = 20
                        def f(y : Int) Int = 30
                        f(0)
                    }""",
                    "30",
                    "Int"
                ),
                ExecTest(
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
                ExecTest(
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
                ExecTest(
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
                ExecTest(
                    "nested def block (outer)",
                    """{
                        def f(x : Int) Int = 10
                        {
                            def g(y : Int) Int = 20
                            f(0)
                        }
                    }""",
                    "10",
                    "Int"
                ),
                ExecTest(
                    "nested def block (inner)",
                    """{
                        def f(x : Int) Int = 10
                        {
                            def g(y : Int) Int = 20
                            g(0)
                        }
                    }""",
                    "20",
                    "Int"
                ),
                ExecTest(
                    "nested def block (redefinition)",
                    """{
                        def f(x : Int) Int = 10
                        {
                            def f(y : Int) Int = 20
                            f(0)
                        }
                    }""",
                    "20",
                    "Int"
                ),
                ExecTest(
                    "variant argument",
                    """{
                        def f (r : <x : Int, y : Int, z : Int>) <x : Int, y : Int, z : Int> = r
                        def g () <x : Int, y : Int> = <x = 3>
                        f(g())
                        }""",
                    "<x = 3>",
                    "<x : Int, y : Int, z : Int>"
                ),

                // Predef

                ExecTest(
                    "true",
                    "true",
                    "<true = {}>",
                    "Boolean",
                    "true"
                ),
                ExecTest(
                    "false",
                    "false",
                    "<false = {}>",
                    "Boolean",
                    "false"
                )

            ) ++ allIntPrimBinOps.flatMap(op => {
                    Vector(
                        ExecTest(
                            s"Pre-defined Ints.${op.name} has the correct type",
                            s"Ints.${op.name}",
                            "<function>",
                            "(Int, Int) Int"
                        ),
                        ExecTest(
                            s"Pre-defined Ints.${op.name} partial application has the correct type",
                            s"Ints.${op.name}(1)",
                            "<function>",
                            "(Int) Int"
                        )
                    )

                }) ++ allIntPrimRelOps.flatMap(op => {
                    Vector(
                        ExecTest(
                            s"Pre-defined Ints.${op.name} has the correct type",
                            s"Ints.${op.name}",
                            "<function>",
                            "(Int, Int) Boolean"
                        ),
                        ExecTest(
                            s"Pre-defined Ints.${op.name} partial application has the correct type",
                            s"Ints.${op.name}(1)",
                            "<function>",
                            "(Int) Boolean"
                        )
                    )
                }) ++ Vector(
                    ExecTest(
                        s"Pre-defined Strings.concat has the correct type",
                        "Strings.concat",
                        "<function>",
                        "(String, String) String"
                    ),
                    ExecTest(
                        s"Pre-defined Strings.concat partial application has the correct type",
                        """Strings.concat("hi")""",
                        "<function>",
                        "(String) String"
                    ),
                    ExecTest(
                        s"Pre-defined Strings.equals has the correct type",
                        "Strings.equals",
                        "<function>",
                        "(String, String) Boolean"
                    ),
                    ExecTest(
                        s"Pre-defined Strings.equals partial application has the correct type",
                        """Strings.equals("hi")""",
                        "<function>",
                        "(String) Boolean"
                    )
                )

        for (aTest <- ExecTests) {
            test(s"${backend.name} run: ${aTest.name}") {
                val result = runString(aTest.name, aTest.program, backend.options, backend)
                result shouldBe ""
            }
            test(s"${backend.name} run: ${aTest.name}: result") {
                val result = runString(aTest.name, aTest.program, Seq("-r") ++ backend.options, backend)
                result shouldBe s"${aTest.expectedCompiledResult}\n"
            }
        }

        // Primitive properties

        def runPrimTest(name : String, args : String, tipe : String, answer : String) : Unit = {
            val code = s"$name($args)"
            val result1 = runString(name, code, backend.options, backend)
            result1 shouldBe ""
            val result2 = runString(name, code, Seq("-r") ++ backend.options, backend)
            result2 shouldBe s"$answer\n"
            val result3 = runREPLOnLine(code, backend.options)
            result3 shouldBe s"""res0 : $tipe = $answer\n"""
        }

        def runBadPrimTest(name : String, args : String, error : String) : Unit = {
            val code = s"$name($args)"
            val result1 = runString(name, code, backend.options, backend)
            result1 shouldBe s"$error\n"
            val result2 = runString(name, code, Seq("-r") ++ backend.options, backend)
            result2 shouldBe s"$error\n"
            val result3 = runREPLOnLine(code, backend.options)
            result3 shouldBe s"$error\n"
        }

        def expectedBool(b : Boolean) : String =
            s"""<${if (b) "true" else "false"} = {}>"""

        case class IntBinPrimTest(
            op : PrimOp,
            func : (BigInt, BigInt) => BigInt
        )

        val intBinPrimTests =
            Vector(
                IntBinPrimTest(ADD, _ + _),
                IntBinPrimTest(MUL, _ * _),
                IntBinPrimTest(SUB, _ - _)
            )

        for (aTest <- intBinPrimTests) {
            val primName = aTest.op.primName
            test(s"${backend.name} run: prim $primName") {
                forAll { (l : BigInt, r : BigInt) =>
                    runPrimTest(s"prim $primName", s"$l, $r", "Int", s"${aTest.func(l, r)}")
                }
            }
        }

        {
            val primName = "IntDiv"
            val func = (l : BigInt, r : BigInt) => l / r

            test(s"${backend.name} run: prim $primName") {
                forAll { (l : BigInt, r : BigInt) =>
                    whenever(r != 0) {
                        runPrimTest(s"prim $primName", s"$l, $r", "Int", s"${func(l, r)}")
                    }
                }
            }

            test(s"${backend.name} run: prim $primName by zero") {
                forAll { (l : BigInt) =>
                    val name = s"prim $primName"
                    val code = s"prim $primName($l, 0)"
                    val result1 = runString(name, code, backend.options, backend)
                    result1 shouldBe "cooma: Error executing integer div: BigInteger divide by zero\n"
                }
            }
        }

        {
            val primName = "IntPow"
            val func = (l : BigInt, r : Int) => l.pow(r)

            test(s"${backend.name} run: prim $primName (non-negative)") {
                forAll { (l : BigInt) =>
                    forAll(Gen.choose(0, 30)) { (r : Int) =>
                        whenever(r >= 0) {
                            runPrimTest(s"prim $primName", s"$l, $r", "Int", s"${func(l, r)}")
                        }
                    }
                }
            }

            test(s"${backend.name} run: prim $primName (negative)") {
                forAll { (l : BigInt) =>
                    forAll(Gen.choose(-30, -1)) { (r : Int) =>
                        whenever(r < 0) {
                            runBadPrimTest(s"prim $primName", s"$l, $r", s"cooma: IntPow: illegal negative power $r given")
                        }
                    }
                }
            }
        }

        case class IntRelPrimTest(
            op : PrimOp,
            func : (BigInt, BigInt) => Boolean
        )

        val intRelPrimTests =
            Vector(
                IntRelPrimTest(EQ, _ == _),
                IntRelPrimTest(NEQ, _ != _),
                IntRelPrimTest(GT, _ > _),
                IntRelPrimTest(GTE, _ >= _),
                IntRelPrimTest(LT, _ < _),
                IntRelPrimTest(LTE, _ <= _)
            )

        for (aTest <- intRelPrimTests) {
            val primName = aTest.op.primName
            test(s"${backend.name} run: prim $primName") {
                forAll { (l : BigInt, r : BigInt) =>
                    runPrimTest(s"prim $primName", s"$l, $r", "Boolean", expectedBool(aTest.func(l, r)))
                }
            }
        }

        val stringLitREStr = """((\\([btnfr]|\\|"))|\w| ){0,40}"""
        val stringLitRE = stringLitREStr.r
        def isStringLit(s : String) = stringLitRE.matches(s)
        val stringLit : Gen[String] = RegexpGen.from(stringLitREStr)

        {
            val primName = "StrConcat"
            val func = (l : String, r : String) => Util.escape(Util.unescape(l) + Util.unescape(r))

            test(s"${backend.name} run: prim $primName") {
                forAll(stringLit, stringLit) { (l : String, r : String) =>
                    whenever(isStringLit(l) && isStringLit(r)) {
                        runPrimTest(s"prim $primName", s""""$l", "$r"""", "String", s""""${func(l, r)}"""")
                    }
                }
            }
        }

        {
            val primName = "StrEquals"
            val func = (l : String, r : String) => Util.unescape(l) == Util.unescape(r)

            test(s"${backend.name} run: prim $primName") {
                forAll(stringLit, stringLit) { (l : String, r : String) =>
                    whenever(isStringLit(l) && isStringLit(r)) {
                        runPrimTest(s"prim $primName", s""""$l", "$r"""", "Boolean", expectedBool(func(l, r)))
                    }
                }
            }
        }

        {
            val primName = "StrLength"
            val func = (s : String) => Util.unescape(s).length

            test(s"${backend.name} run: prim $primName") {
                forAll(stringLit) { (s : String) =>
                    whenever(isStringLit(s)) {
                        runPrimTest(s"prim $primName", s""""$s"""", "Int", s"${func(s)}")
                    }
                }
            }
        }

        {
            val primName = "StrSubstr"
            val func = (s : String, i : BigInt) => Util.escape(s.substring(i.toInt))
            def indexesOf(s : String) : Gen[Int] =
                for {
                    n <- Gen.choose(0, s.length)
                } yield n
            def notIndexesOf(s : String) : Gen[Int] =
                for {
                    n <- Gen.oneOf(Gen.negNum[Int], Gen.posNum[Int] suchThat (_ > s.length))
                } yield n

            test(s"${backend.name} run: prim $primName") {
                forAll(stringLit) { (l : String) =>
                    whenever(isStringLit(l)) {
                        val s = Util.unescape(l)
                        forAll(indexesOf(s)) { (i : Int) =>
                            whenever((i >= 0) && (i <= s.length)) {
                                runPrimTest(s"prim $primName", s""""$l", $i""", "String", s""""${func(s, i)}"""")
                            }
                        }
                    }
                }
            }

            test(s"${backend.name} run: prim $primName bad index") {
                forAll(stringLit) { (l : String) =>
                    whenever(isStringLit(l)) {
                        val s = Util.unescape(l)
                        forAll(notIndexesOf(s)) { (i : Int) =>
                            whenever((i < 0) || (i > s.length)) {
                                runBadPrimTest(s"prim $primName", s""""$l", $i""",
                                    s"""cooma: StrSubstr: index $i out of range for string "$s"""")
                            }
                        }
                    }
                }
            }

        }

        // Options

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

        for (aTest <- ExecTests) {
            test(s"${backend.name} REPL: ${aTest.name}") {
                val result = runREPLOnLine(aTest.program, backend.options)
                val expectedResult = s"${aTest.expectedREPLVar} : ${aTest.expectedREPLType} = ${aTest.expectedCompiledResult}\n"
                result shouldBe expectedResult
            }
        }

        case class REPLTest(
            name : String,
            program : String,
            expectedResult : String
        )

        val replTests =
            Vector(
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
                    "single evaluation (function and use)",
                    """
                        fun (x : Int) x
                        res0(10)
                    """,
                    "res0 : (Int) Int = <function>\nres1 : Int = 10"
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
                    "x : Int = 1\nx : Int = 1"
                ),
                REPLTest(
                    "multiple value definitions (upper)",
                    """
                        val x = 1
                        val y = 2
                        x
                    """,
                    "x : Int = 1\ny : Int = 2\nx : Int = 1"
                ),
                REPLTest(
                    "multiple value definitions (lower)",
                    """
                        val x = 1
                        val y = 2
                        y
                    """,
                    "x : Int = 1\ny : Int = 2\ny : Int = 2"
                ),
                REPLTest(
                    "multiple value definitions (redefinition)",
                    """
                        val x = 1
                        val x = 2
                        x
                    """,
                    "x : Int = 1\nx : Int = 2\nx : Int = 2"
                ),
                REPLTest(
                    "single function definition",
                    """
                        def f(x : Int) Int = x
                        f(10)
                    """,
                    "f : (Int) Int = <function>\nres0 : Int = 10"
                ),
                REPLTest(
                    "value and function definition",
                    """
                        val x = 10
                        def f(y : Int) Int = x
                        f(20)
                    """,
                    "x : Int = 10\nf : (Int) Int = <function>\nres0 : Int = 10"
                ),
                REPLTest(
                    "multiple function definitions (upper)",
                    """
                        def f(x : Int) Int = 10
                        def g(y : Int) Int = 20
                        f(1)
                    """,
                    "f : (Int) Int = <function>\ng : (Int) Int = <function>\nres0 : Int = 10"
                ),
                REPLTest(
                    "multiple function definitions (lower)",
                    """
                        def f(x : Int) Int = 10
                        def g(y : Int) Int = 20
                        g(1)
                    """,
                    "f : (Int) Int = <function>\ng : (Int) Int = <function>\nres0 : Int = 20"
                ),
                REPLTest(
                    "multiple function definitions (chain)",
                    """
                        def f(x : Int) Int = 10
                        def g(y : Int) Int = f(y)
                        g(1)
                    """,
                    "f : (Int) Int = <function>\ng : (Int) Int = <function>\nres0 : Int = 10"
                ),
                REPLTest(
                    "single result name binding from constant",
                    """
                        10
                        res0
                    """,
                    "res0 : Int = 10\nres0 : Int = 10"
                ),
                REPLTest(
                    "single result name binding from val",
                    """
                        val x = 10
						x
                    """,
                    "x : Int = 10\nx : Int = 10"
                ),
                REPLTest(
                    "multiple result name binding",
                    """
                        10
                        20
                        30
                        res2
                    """,
                    "res0 : Int = 10\nres1 : Int = 20\nres2 : Int = 30\nres2 : Int = 30"
                ),
                REPLTest(
                    "built-in type",
                    "Int",
                    "res0 : Type"
                ),
                REPLTest(
                    "pre-defined type",
                    "Boolean",
                    "Boolean : Type"
                ),
                REPLTest(
                    "pre-defined value",
                    "true",
                    "true : Boolean = <true = {}>"
                )
            )

        for (aTest <- replTests) {
            test(s"${backend.name} REPL: ${aTest.name}") {
                val result = runREPLOnLines(aTest.program, backend.options)
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

        {
            val filename = "src/test/resources/primitives/intAdd.cooma"
            val name = s"Primitives file execution($filename)"
            val expectedResult = "0\n"

            test(s"${backend.name} run: $name") {
                val result = runFile(filename, backend.options, backend, Seq())
                result shouldBe ""
            }

            test(s"${backend.name} run: $name result") {
                val result = runFile(filename, backend.options ++ Seq("-r"), backend, Seq())
                result shouldBe expectedResult
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

    def runTest(tester : Config => Unit, options : Seq[String], args : Seq[String]) : String = {
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
        runTest(backend.frontend.interpret(name, program, _), options, allArgs)
    }

    def runFile(program : String, options : Seq[String], backend : Backend, args : Seq[String]) : String = {
        val allArgs = Seq("--Koutput", "string") ++ options ++ (program +: args)
        runTest(backend.frontend.interpret, options, allArgs)
    }

    def runREPLTest(cmd : String, input : String, options : Seq[String]) : String = {
        val allArgs = Seq("--Koutput", "string") ++ options
        val config = makeConfig(allArgs)
        val replInput =
            if (input.indexOf('\n') == -1)
                input
            else
                s"$cmd\n$input\n:end"
        val console = new StringConsole(replInput)
        val repl = createREPL(config)
        runTest(repl.processconsole(console, "dummy", _), options, allArgs)
    }

    def runREPLOnLine(input : String, options : Seq[String]) : String =
        runREPLTest(":paste", input, options)

    def runREPLOnLines(input : String, options : Seq[String]) : String =
        runREPLTest(":lines", input, options)

    override def createREPL(config : Config) : REPL with Compiler with org.bitbucket.inkytonik.cooma.Backend = {
        val repl =
            if (config.graalVM())
                new TruffleBackend(config) with TruffleREPL with Compiler
            else {
                val source = new StringSource("")
                new ReferenceBackend(this, source, config) with ReferenceREPL with Compiler
            }
        repl.initialise()
        repl
    }

    override def process(source : Source, prog : Program, config : Config) : Unit = {
        val frontend =
            if (config.graalVM())
                new TruffleDriver
            else
                new ReferenceDriver
        frontend.process(source, prog, config)
    }

    override def testdriver(config : Config) : Unit = {
        if (config.graalVM())
            new TruffleFrontend().interpret(config)
        else
            super.testdriver(config)
    }
}
