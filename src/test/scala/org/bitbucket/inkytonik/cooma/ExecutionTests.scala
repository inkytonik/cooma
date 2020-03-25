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
    import java.nio.file.{Files, Paths}

    import org.bitbucket.inkytonik.cooma.Primitives._
    import org.bitbucket.inkytonik.cooma.backend.ReferenceBackend
    import org.bitbucket.inkytonik.cooma.truffle.{TruffleBackend, TruffleDriver, TruffleFrontend, TruffleREPL}
    import org.bitbucket.inkytonik.kiama.util.Filenames.makeTempFilename
    import org.bitbucket.inkytonik.kiama.util.IO.{createFile, deleteFile}
    import org.bitbucket.inkytonik.kiama.util.{FileSource, Source, StringConsole, StringSource}
    import org.rogach.scallop.throwError
    import org.scalacheck.Gen
    import wolfendale.scalacheck.regexp.RegexpGen

    case class Backend(
        name : String,
        options : Seq[String],
        var frontend : Frontend
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

        case class ExecTestError(
            name : String,
            program : String,
            expectedErrorMessage : String
        )

        val execTests =
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
                    "{ x = 65 }",
                    "{ x = 65 }",
                    "{ x : Int }"
                ),
                ExecTest(
                    "record (single string field)",
                    """{ name = "Harold" }""",
                    """{ name = "Harold" }""",
                    "{ name : String }"
                ),
                ExecTest(
                    "record (two fields)",
                    "{ a = 1, b = 2 }",
                    "{ a = 1, b = 2 }",
                    "{ a : Int, b : Int }"
                ),
                ExecTest(
                    "record (many fields)",
                    """{ name = "Bob", age = 24, year = 1998, sex = "F" }""",
                    """{ name = "Bob", age = 24, year = 1998, sex = "F" }""",
                    "{ name : String, age : Int, year : Int, sex : String }"
                ),
                ExecTest(
                    "record (eval field)",
                    "{ a = {fun (x : Int) x}(3), b = 2 }",
                    "{ a = 3, b = 2 }",
                    "{ a : Int, b : Int }"
                ),
                ExecTest(
                    "multi-line record",
                    """{
                        name = "Bob",
                        age = 24
                    }""",
                    """{ name = "Bob", age = 24 }""",
                    "{ name : String, age : Int }"
                ),
                ExecTest(
                    "field select (first of one)",
                    """{ s = "Hi" }.s""",
                    """"Hi"""",
                    "String"
                ),
                ExecTest(
                    "field select (first of two)",
                    """{ s = "Hi", t = 10 }.s""",
                    """"Hi"""",
                    "String"
                ),
                ExecTest(
                    "field select (second of two)",
                    """{ s = "Hi", t = 10 }.t""",
                    "10",
                    "Int"
                ),
                ExecTest(
                    "field select (many fields)",
                    """{ name = "Bob", age = 24, year = 1998, sex = "F" }.sex""",
                    """"F"""",
                    "String"
                ),
                ExecTest(
                    "nested field select",
                    "{ r = { y = 42 } }.r.y",
                    "42",
                    "Int"
                ),
                ExecTest(
                    "record concatenation",
                    """{
                        val r = { x = 10, y = 20 }
                        val s = { a = "Hi" }
                        r & s
                    }""",
                    """{ x = 10, y = 20, a = "Hi" }""",
                    "{ x : Int, y : Int, a : String }"
                ),
                ExecTest(
                    "select from record concatenation (left)",
                    """{
                       val r = { x = 10, y = 20 }
                       val s = { a = "Hi" }
                       {r & s}.x
                   }""",
                    "10",
                    "Int"
                ),
                ExecTest(
                    "select from record concatenation (right)",
                    """{
                       val r = { x = 10, y = 20 }
                       val s = { a = "Hi" }
                       {r & s}.a
                   }""",
                    """"Hi"""",
                    "String"
                ),

                // Variants

                ExecTest(
                    "variant (single int field)",
                    "< x = 65 >",
                    "< x = 65 >",
                    "< x : Int >"
                ),
                ExecTest(
                    "variant (single string field)",
                    """< name = "Harold" >""",
                    """< name = "Harold" >""",
                    "< name : String >"
                ),
                ExecTest(
                    "variant (eval field)",
                    "< a = {fun (x : Int) x}(3) >",
                    "< a = 3 >",
                    "< a : Int >"
                ),
                ExecTest(
                    "multi-line variant",
                    """<
                        name = "Bob"
                    >""",
                    """< name = "Bob" >""",
                    "< name : String >"
                ),
                ExecTest(
                    "basic match",
                    "< x = 1 > match { case x(a) => a }",
                    "1",
                    "Int"
                ),
                ExecTest(
                    "multi-case match (first case, same order)",
                    """{
                        def f () < x : Int, y : Int > = < x = 3 >
                        f () match { case x(a) => 1 case y(b) => 2 }
                    }""",
                    "1",
                    "Int"
                ),
                ExecTest(
                    "multi-case match (later case, same order)",
                    """{
                        def f () < x : Int, y : Int > = < y = 3 >
                        f () match { case x(a) => 1 case y(b) => 2 }
                    }""",
                    "2",
                    "Int"
                ),
                ExecTest(
                    "multi-case match (first case, different order)",
                    """{
                        def f () < x : Int, y : Int > = < y = 3 >
                        f () match { case y(b) => 1 case x(a) => 2 }
                    }""",
                    "1",
                    "Int"
                ),
                ExecTest(
                    "multi-case match (later case, different order)",
                    """{
                        def f () < x : Int, y : Int > = < x = 3>
                        f () match { case y(b) => 1 case x(a) => 2 }
                    }""",
                    "2",
                    "Int"
                ),
                ExecTest(
                    "Boolean match",
                    """{
                        def f (b : Boolean) Unit =
                            b match {
                                case False(x) => x
                                case True(x) => x
                            }
                        { a = f(false), b = f(true) }
                    }""",
                    "{ a = {}, b = {} }",
                    "{ a : Unit, b : Unit }"
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
                    "{fun (r : { x : Int }) r.x}({ x = 20 })",
                    "20",
                    "Int"
                ),
                ExecTest(
                    "single field record return",
                    "{fun (x : Int) { a = x }}(9)",
                    "{ a = 9 }",
                    "{ a : Int }"
                ),
                ExecTest(
                    "variant argument (one)",
                    "{fun (r : < x : Int >) r}(< x = 20 >)",
                    "< x = 20 >",
                    "< x : Int >"
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
                ExecTest(
                    "function program result (type arg)",
                    "{fun (t : Type, x : t) x}",
                    "<function>",
                    "(t : Type, x : t) t"
                ),
                ExecTest(
                    "partial type application",
                    "{fun (t : Type, x : t) x}(Int)",
                    "<function>",
                    "(x : Int) Int"
                ),
                ExecTest(
                    "type application (fun)",
                    "{fun (t : Type, x : t) x}(Int, 10)",
                    "10",
                    "Int"
                ),
                ExecTest(
                    "another type application (fun)",
                    """{fun (t : Type, x : t) x}(String, "hi")""",
                    """"hi"""",
                    "String"
                ),
                ExecTest(
                    "type application at different types",
                    """{
                        def id(t : Type, x : t) t = x
                        {
                            b = id(Boolean, true),
                            i = id(Int, 10),
                            s = id(String, "hello"),
                            r = id(Reader, { read = fun () "hello" })
                        }
                    }""",
                    """{ b = true, i = 10, s = "hello", r = { read = <function> } }""",
                    "{ b : Boolean, i : Int, s : String, r : Reader }"
                ),
                ExecTest(
                    "type application (def)",
                    """{def id(t : Type, x : t) t = x id(String, "hi")}""",
                    """"hi"""",
                    "String"
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
                    "def block (self reference, tail call)",
                    """{
                        def f(x : Int) Int =
                            predef.equal(Int, x, 0) match {
                                case True(_)  => 20
                                case False(_) => f(predef.Ints.sub(x, 1))
                            }
                        f(10)
                    }""",
                    "20",
                    "Int"
                ),
                ExecTest(
                    "def block (accumulator, tail call)",
                    """{
                        def f(s : Int, x : Int) Int =
                            predef.equal(Int, x, 0) match {
                                case True(_)  => s
                                case False(_) => f(predef.Ints.add(s, x), predef.Ints.sub(x, 1))
                            }
                        f(0, 10)
                    }""",
                    "55",
                    "Int"
                ),
                ExecTest(
                    "def block (multi forward reference)",
                    """{
                        def f(x : Int) Int = g(x)
                        def g(y : Int) Int = y
                        f(10)
                    }""",
                    "10",
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
                        def f (r : < x : Int, y : Int, z : Int >) < x : Int, y : Int, z : Int > = r
                        def g () < x : Int, y : Int > = < x = 3 >
                        f(g())
                        }""",
                    "< x = 3 >",
                    "< x : Int, y : Int, z : Int >"
                ),

                // Predef

                ExecTest(
                    "true",
                    "true",
                    "true",
                    "Boolean",
                    "true"
                ),
                ExecTest(
                    "false",
                    "false",
                    "false",
                    "Boolean",
                    "false"
                ),
                ExecTest(
                    "predef.Booleans.and(false, false)",
                    "predef.Booleans.and(false, false)",
                    "false",
                    "Boolean"
                ),
                ExecTest(
                    "predef.Booleans.and(false, true)",
                    "predef.Booleans.and(false, true)",
                    "false",
                    "Boolean"
                ),
                ExecTest(
                    "predef.Booleans.and(true, false)",
                    "predef.Booleans.and(true, false)",
                    "false",
                    "Boolean"
                ),
                ExecTest(
                    "predef.Booleans.and(true, true)",
                    "predef.Booleans.and(true, true)",
                    "true",
                    "Boolean"
                ),
                ExecTest(
                    "predef.Booleans.not(false)",
                    "predef.Booleans.not(false)",
                    "true",
                    "Boolean"
                ),
                ExecTest(
                    "predef.Booleans.not(true)",
                    "predef.Booleans.not(true)",
                    "false",
                    "Boolean"
                ),

                ExecTest(
                    "predef.Booleans.or(false, false)",
                    "predef.Booleans.or(false, false)",
                    "false",
                    "Boolean"
                ),
                ExecTest(
                    "predef.Booleans.or(false, true)",
                    "predef.Booleans.or(false, true)",
                    "true",
                    "Boolean"
                ),
                ExecTest(
                    "predef.Booleans.or(true, false)",
                    "predef.Booleans.or(true, false)",
                    "true",
                    "Boolean"
                ),
                ExecTest(
                    "predef.Booleans.or(true, true)",
                    "predef.Booleans.or(true, true)",
                    "true",
                    "Boolean"
                ),
                ExecTest(
                    "Booleans",
                    "predef.Booleans",
                    "{ and = <function>, not = <function>, or = <function> }",
                    """{
                       |    and : (l : Boolean, r : Boolean) Boolean,
                       |    not : (b : Boolean) Boolean,
                       |    or : (l : Boolean, r : Boolean) Boolean
                       |}""",
                    "res0"
                ),
                ExecTest(
                    "Ints",
                    "predef.Ints",
                    """{
                      |    abs = <function>,
                      |    add = <function>,
                      |    div = <function>,
                      |    mul = <function>,
                      |    pow = <function>,
                      |    sub = <function>,
                      |    lt = <function>,
                      |    lte = <function>,
                      |    gt = <function>,
                      |    gte = <function>,
					  |    max = <function>,
					  |    mod = <function>
                      |}""",
                    """{
					  |    abs : (x : Int) Int,
                      |    add : (x : Int, y : Int) Int,
                      |    div : (x : Int, y : Int) Int,
                      |    mul : (x : Int, y : Int) Int,
                      |    pow : (x : Int, y : Int) Int,
                      |    sub : (x : Int, y : Int) Int,
                      |    lt : (x : Int, y : Int) Boolean,
                      |    lte : (x : Int, y : Int) Boolean,
                      |    gt : (x : Int, y : Int) Boolean,
                      |    gte : (x : Int, y : Int) Boolean,
					  |    max : (x : Int, y : Int) Int,
					  |    mod : (x : Int, y : Int) Int
                      |}""",
                ),
                ExecTest(
                    "< v = predef.Ints >",
                    "< v = predef.Ints >",
                    """< v = {
                       |    abs = <function>,
                       |    add = <function>,
                       |    div = <function>,
                       |    mul = <function>,
                       |    pow = <function>,
                       |    sub = <function>,
                       |    lt = <function>,
                       |    lte = <function>,
                       |    gt = <function>,
                       |    gte = <function>,
					   |    max = <function>,
                       |    mod = <function>
                       |} >""",
                    """<
                       |    v : {
                       |        abs : (x : Int) Int,
                       |        add : (x : Int, y : Int) Int,
                       |        div : (x : Int, y : Int) Int,
                       |        mul : (x : Int, y : Int) Int,
                       |        pow : (x : Int, y : Int) Int,
                       |        sub : (x : Int, y : Int) Int,
                       |        lt : (x : Int, y : Int) Boolean,
                       |        lte : (x : Int, y : Int) Boolean,
                       |        gt : (x : Int, y : Int) Boolean,
                       |        gte : (x : Int, y : Int) Boolean,
					   |        max : (x : Int, y : Int) Int,
                       |        mod : (x : Int, y : Int) Int
                       |    }
                       |>"""
                ),
                ExecTest(
                    "{ x = { a = 1, b = predef.Ints } }",
                    "{ x = { a = 1, b = predef.Ints } }",
                    """{
                       |    x = {
                       |        a = 1,
                       |        b = {
                       |            abs = <function>,
                       |            add = <function>,
                       |            div = <function>,
                       |            mul = <function>,
                       |            pow = <function>,
                       |            sub = <function>,
                       |            lt = <function>,
                       |            lte = <function>,
                       |            gt = <function>,
                       |            gte = <function>,
					   |            max = <function>,
                       |            mod = <function>
                       |        }
                       |    }
                       |}""",
                    """{
                       |    x : {
                       |        a : Int,
                       |        b : {
                       |            abs : (x : Int) Int,
                       |            add : (x : Int, y : Int) Int,
                       |            div : (x : Int, y : Int) Int,
                       |            mul : (x : Int, y : Int) Int,
                       |            pow : (x : Int, y : Int) Int,
                       |            sub : (x : Int, y : Int) Int,
                       |            lt : (x : Int, y : Int) Boolean,
                       |            lte : (x : Int, y : Int) Boolean,
                       |            gt : (x : Int, y : Int) Boolean,
                       |            gte : (x : Int, y : Int) Boolean,
					   |            max : (x : Int, y : Int) Int,
                       |            mod : (x : Int, y : Int) Int
                       |        }
                       |    }
                       |}"""
                ),
                ExecTest(
                    "equal has the correct type",
                    "predef.equal",
                    "<function>",
                    "(t : Type, l : t, r : t) Boolean"
                ),
                ExecTest(
                    "equality of integers (equal)",
                    "predef.equal(Int, 42, 42)",
                    "true",
                    "Boolean"
                ),
                ExecTest(
                    "equality of integers (unequal)",
                    "predef.equal(Int, 42, 99)",
                    "false",
                    "Boolean"
                ),
                ExecTest(
                    "equality of strings (equal)",
                    s"""predef.equal(String, "abc", "abc")""",
                    "true",
                    "Boolean"
                ),
                ExecTest(
                    "equality of strings (unequal)",
                    s"""predef.equal(String, "abc", "cba")""",
                    "false",
                    "Boolean"
                ),
                ExecTest(
                    "equality of Booleans (equal)",
                    "predef.equal(Boolean, true, true)",
                    "true",
                    "Boolean"
                ),
                ExecTest(
                    "equality of Booleans (unequal)",
                    "predef.equal(Boolean, true, false)",
                    "false",
                    "Boolean"
                ),
                ExecTest(
                    "equality of records (equal, flat)",
                    "predef.equal({x : Int, y : Int}, {x = 0, y = 1}, {y = 1, x = 0})",
                    "true",
                    "Boolean"
                ),
                ExecTest(
                    "equality of records (equal, nested)",
                    "predef.equal({x : { a : Int, b : Int }, y : Int}, {x = {a = 0, b = 0}, y = 1}, {y = 1, x = {b = 0, a = 0}})",
                    "true",
                    "Boolean"
                ),
                ExecTest(
                    "equality of records (unequal, flat",
                    "predef.equal({x : Int, y : Int}, {x = 0, y = 0}, {y = 1, x = 0})",
                    "false",
                    "Boolean"
                ),
                ExecTest(
                    "equality of records (unequal, nested)",
                    "predef.equal({x : { a : Int, b : Int }, y : Int}, {x = {a = 0, b = 0}, y = 1}, {y = 1, x = {b = 1, a = 0}})",
                    "false",
                    "Boolean"
                ),
                ExecTest(
                    "equality of variants (equal, flat)",
                    "predef.equal(< a : Int, v : String >, < a = 1 >, < a = 1 >)",
                    "true",
                    "Boolean"
                ),
                ExecTest(
                    "equality of variants (equal, nested)",
                    "predef.equal(< a : { x : Int, y : Int }, v : String >, < a = {x = 1, y = 2} >, < a = {y = 2, x = 1} >)",
                    "true",
                    "Boolean"
                ),
                ExecTest(
                    "equality of variants (unequal, same constructor)",
                    "predef.equal(< a : Int, v : Int >, < a = 1 >, < a = 2 >)",
                    "false",
                    "Boolean"
                ),
                ExecTest(
                    "equality of variants (unequal, different constructor)",
                    "predef.equal(< a : Int, v : Int >, < a = 1 >, < v = 2 >)",
                    "false",
                    "Boolean"
                ),
                ExecTest(
                    "equality of variants (unequal, nested)",
                    "predef.equal(< a : { x : Int, y : Int }, v : String >, < a = {x = 1, y = 2} >, < a = {y = 2, x = 2} >)",
                    "false",
                    "Boolean"
                )

            ) ++ allInt1PrimBinOps.flatMap(op => {
                    Vector(
                        ExecTest(
                            s"Pre-defined Ints.${op.name} has the correct type",
                            s"predef.Ints.${op.name}",
                            "<function>",
                            "(x : Int) Int"
                        )
                    )
                }) ++ allInt2PrimBinOps.flatMap(op => {
                    Vector(
                        ExecTest(
                            s"Pre-defined Ints.${op.name} has the correct type",
                            s"predef.Ints.${op.name}",
                            "<function>",
                            "(x : Int, y : Int) Int"
                        ),
                        ExecTest(
                            s"Pre-defined Ints.${op.name} partial application has the correct type",
                            s"predef.Ints.${op.name}(1)",
                            "<function>",
                            "(y : Int) Int"
                        )
                    )

                }) ++ allIntPrimRelOps.flatMap(op => {
                    Vector(
                        ExecTest(
                            s"Pre-defined Ints.${op.name} has the correct type",
                            s"predef.Ints.${op.name}",
                            "<function>",
                            "(x : Int, y : Int) Boolean"
                        ),
                        ExecTest(
                            s"Pre-defined Ints.${op.name} partial application has the correct type",
                            s"predef.Ints.${op.name}(1)",
                            "<function>",
                            "(y : Int) Boolean"
                        )
                    )
                }) ++ Vector(
                    ExecTest(
                        s"Pre-defined Strings.concat has the correct type",
                        "predef.Strings.concat",
                        "<function>",
                        "(x : String, y : String) String"
                    ),
                    ExecTest(
                        s"Pre-defined Strings.concat partial application has the correct type",
                        """predef.Strings.concat("hi")""",
                        "<function>",
                        "(y : String) String"
                    ),
                    ExecTest(
                        s"Pre-defined Strings.length has the correct type",
                        "predef.Strings.length",
                        "<function>",
                        "(x : String) Int"
                    ),
                    ExecTest(
                        s"Pre-defined Strings.substr has the correct type",
                        "predef.Strings.substr",
                        "<function>",
                        "(x : String, y : Int) String"
                    ),
                    ExecTest(
                        s"Pre-defined Strings.substr partial application has the correct type",
                        """predef.Strings.substr("hi")""",
                        "<function>",
                        "(y : Int) String"
                    ),
                    //Vector tests
                    ExecTest(
                        s"Vector literal",
                        """[1,2,3]""",
                        "[1,2,3]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Int Vector declaration",
                        """{
                            val x : Vector(Int) = [1,2,3]
                            x
                        }""",
                        "[1,2,3]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Empty Int Vector declaration",
                        """{
                            val x : Vector(Int) = []
                            x
                        }""",
                        "[]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Multi-dimensional vector declaration",
                        """{
                            val x : Vector(Vector(Int)) = [[1,2,3],
														   [4,5,6],
				                                           [7,8,9]]
                            x
                        }""",
                        "[[1,2,3],[4,5,6],[7,8,9]]",
                        "Vector(Vector(Int))"
                    ),
                    ExecTest(
                        "Record vector declaration",
                        """{
                            val x : Vector({ a : Int }) = [{ a = 65 },
														   { a = -50 }]
                            x
                        }""",
                        "[{ a = 65 },{ a = -50 }]",
                        "Vector({ a : Int })"
                    ),
                    ExecTest(
                        "Boolean vector declaration",
                        """{
                            val x : Vector(Boolean) = [true, false]
                            x
                        }""",
                        "[true,false]",
                        "Vector(Boolean)"
                    ),
                    ExecTest(
                        "Boolean vector declaration - cont.",
                        """{
                            val x : Vector(Boolean) = [predef.Booleans.and(false, false),
                            						   predef.Booleans.and(false, true),
                            						   predef.Booleans.and(true, false),
                            						   predef.Booleans.and(true, true)]
                            x
                        }""",
                        "[false,false,false,true]",
                        "Vector(Boolean)"
                    ),
                    ExecTest(
                        "Unit Vector declaration",
                        """{
                            val x : Vector(Unit) = [{}]
                            x
                        }""",
                        "[{}]",
                        "Vector(Unit)"
                    ),
                    ExecTest(
                        "String Vector declaration",
                        """{
                            val x : Vector(String) = ["hello", "world"]
                            x
                        }""",
                        "[\"hello\",\"world\"]",
                        "Vector(String)"
                    ),
                    ExecTest(
                        "Variant Vector declaration",
                        """{
                            val x : Vector(< a : Int >) = [< a = 1 >]
                            x
                        }""",
                        "[< a = 1 >]",
                        "Vector(< a : Int >)"
                    ),
                    ExecTest(
                        "Vector operations - get",
                        """{
                            val x : Vector(Int) = [1,2,3]
                            predef.Vectors.get(Int, x, 0)
                        }""",
                        "1",
                        "Int"
                    ),
                    ExecTest(
                        "Vector operations - get - upper bound",
                        """{
                            val x : Vector(Int) = [1,2,3]
                            predef.Vectors.get(Int, x, 2)
                        }""",
                        "3",
                        "Int"
                    ),
                    ExecTest(
                        "Vector operations - append",
                        """{
                            val x : Vector(Int) = [1,2,3]
                            val z = predef.Vectors.append(Int, x, 4)
							predef.Vectors.get(Int, z, 3)
                        }""",
                        "4",
                        "Int"
                    ),
                    ExecTest(
                        "Vector operations - append on empty vector",
                        """{
                            val x : Vector(Int) = []
							predef.Vectors.get(Int, predef.Vectors.append(Int, x, 4), 0)
                        }""",
                        "4",
                        "Int"
                    ),
                    ExecTest(
                        "Vector operations - put on vector",
                        """{
                            val x : Vector(Int) = [1]
							predef.Vectors.put(Int, x, 0, 5)
                        }""",
                        "[5]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - empty vector length",
                        """{
                            val x  = []
							predef.Vectors.length(Int, x)
                        }""",
                        "0",
                        "Int"
                    ),
                    ExecTest(
                        "Vector operations - vector length",
                        """{
                            val x  = [1,2,3]
							predef.Vectors.length(Int, x)
                        }""",
                        "3",
                        "Int"
                    ),
                    ExecTest(
                        "Vector operations - vector empty slice",
                        """{
                            val x  = [1,2,3]
							predef.Vectors.slice(Int, x,0,0)
                        }""",
                        "[]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - empty vector  slice",
                        """{
                            val x  = []
							predef.Vectors.slice(Int, x,0,0)
                        }""",
                        "[]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - vector slice",
                        """{
                            val x  = [1,2,3]
							predef.Vectors.slice(Int, x,0,1)
                        }""",
                        "[1]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - vector slice 2",
                        """{
                            val x  = [1,2,3]
							predef.Vectors.slice(Int, x,0,3)
                        }""",
                        "[1,2,3]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - vector slice 3",
                        """{
                            val x  = [1,2,3,4]
							predef.Vectors.slice(Int, x,1,3)
                        }""",
                        "[2,3]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - vector slice 4",
                        """{
                            val x  = [1,2,3,4]
							predef.Vectors.slice(Int, x,2,4)
                        }""",
                        "[3,4]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - empty concat - both",
                        """{
                            val x : Vector(Int) = []
							val y : Vector(Int) = []
							predef.Vectors.concat(Int, x, y)
                        }""",
                        "[]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - empty concat - left",
                        """{
                            val x : Vector(Int) = []
							val y : Vector(Int) = [1]
							predef.Vectors.concat(Int, x, y)
                        }""",
                        "[1]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - empty concat - right",
                        """{
                            val x : Vector(Int) = [1]
							val y : Vector(Int) = []
							predef.Vectors.concat(Int, x, y)
                        }""",
                        "[1]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - concat ",
                        """{
                            val x : Vector(Int) = [1,2,3]
							val y : Vector(Int) = [4,5,6]
							predef.Vectors.concat(Int, x, y)
                        }""",
                        "[1,2,3,4,5,6]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - map ",
                        """{
                                predef.map(Int, [1,2,3], fun (x : Int) predef.Ints.add(x, 1))
                            }""",
                        "[2,3,4]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - map - identity function ",
                        """{
                                predef.map(Int, [1,2,3], fun (x : Int) x)
                            }""",
                        "[1,2,3]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - map - empty vector",
                        """{
                                predef.map(Int, [] , fun (x : Int) predef.Ints.add(x, 1))
                            }""",
                        "[]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - fold ",
                        """{
                                predef.fold(Int, [1,2,3], fun (l : Int, r : Int) predef.Ints.add(l, r), 0)
                            }""",
                        "6",
                        "Int"
                    ),
                    ExecTest(
                        "Vector operations - fold - starting from 10",
                        """{
                                predef.fold(Int, [1,2,3], fun (l : Int, r : Int) predef.Ints.add(l, r), 10)
                            }""",
                        "16",
                        "Int"
                    ),
                    ExecTest(
                        "Vector operations - fold - empty vector",
                        """{
                                predef.fold(Int, [], fun (l : Int, r : Int) predef.Ints.add(l, r), 10)
                            }""",
                        "10",
                        "Int"
                    ),
                    ExecTest(
                        "Vector operations - fold left",
                        """{
                                predef.foldLeft(Int, Int,  [1,2,3], fun (l : Int, r : Int) predef.Ints.sub(l, r), 0)
                            }""",
                        "-6",
                        "Int"
                    ),
                    ExecTest(
                        "Vector operations - fold left - empty vector",
                        """{
                                predef.foldLeft(Int, Int, [], fun (l : Int, r : Int) predef.Ints.sub(l, r), 0)
                            }""",
                        "0",
                        "Int"
                    ),
                    ExecTest(
                        "Vector operations - fold left - empty vector from 10",
                        """{
                                predef.foldLeft(Int, Int, [], fun (l : Int, r : Int) predef.Ints.sub(l, r), 10)
                            }""",
                        "10",
                        "Int"
                    ),
                    ExecTest(
                        "Vector operations - fold left - one element vector from 10",
                        """{
                                predef.foldLeft(Int, Int, [0], fun (l : Int, r : Int) predef.Ints.sub(l, r), 10)
                            }""",
                        "10",
                        "Int"
                    ),
                    ExecTest(
                        "Vector operations - fold left - all elements greater than 1 - false",
                        """{
                                predef.foldLeft(Int, Boolean, [1,2,3], fun (l : Boolean, r : Int) predef.Booleans.and(l, predef.Ints.gt(r,1)), true)
                            }""",
                        "false",
                        "Boolean"
                    ),
                    ExecTest(
                        "Vector operations - fold left - all elements greater than 1 - true",
                        """{
                                predef.foldLeft(Int, Boolean, [2,3,4], fun (l : Boolean, r : Int) predef.Booleans.and(l, predef.Ints.gt(r,1)), true)
                            }""",
                        "true",
                        "Boolean"
                    ),
                    ExecTest(
                        "Vector operations - fold right - empty vector",
                        """{
                                predef.foldRight(Int, Int, [], fun (l : Int, r : Int) predef.Ints.sub(l, r), 0)
                            }""",
                        "0",
                        "Int"
                    ),
                    ExecTest(
                        "Vector operations - fold right - empty vector from 10",
                        """{
                                predef.foldRight(Int, Int, [], fun (l : Int, r : Int) predef.Ints.sub(l, r), 10)
                            }""",
                        "10",
                        "Int"
                    ),
                    ExecTest(
                        "Vector operations - fold right - one element vector from 10",
                        """{
                                predef.foldRight(Int, Int, [0], fun (l : Int, r : Int) predef.Ints.sub(l, r), 10)
                            }""",
                        "-10",
                        "Int"
                    ),
                    ExecTest(
                        "Vector operations - fold right",
                        """{
                                predef.foldRight(Int, Int,  [1,2,3], fun (l : Int, r : Int) predef.Ints.sub(l, r), 0)
                            }""",
                        "2",
                        "Int"
                    ),
                    ExecTest(
                        "Vector operations - fold right - all elements greater than 1 - false",
                        """{
                                predef.foldRight(Int, Boolean, [1,2,3], fun (l : Int, r : Boolean) predef.Booleans.and(r, predef.Ints.gt(l,1)), true)
                            }""",
                        "false",
                        "Boolean"
                    ),
                    ExecTest(
                        "Vector operations - fold right - all elements greater than 1 - true",
                        """{
                                predef.foldRight(Int, Boolean, [2,3,4], fun (l : Int, r : Boolean) predef.Booleans.and(r, predef.Ints.gt(l,1)), true)
                            }""",
                        "true",
                        "Boolean"
                    ),
                    ExecTest(
                        "Vector operations - reduce sum",
                        """{
                                predef.reduce(Int, [2,3,4],  predef.Ints.add)
                            }""",
                        "9",
                        "Int"
                    ),
                    ExecTest(
                        "Vector operations - reduce max",
                        """{
                                predef.reduce(Int, [1,2,3,2,1,0,-1,-2,10,1,2,3,4],  predef.Ints.max)
                            }""",
                        "10",
                        "Int"
                    ),
                    ExecTest(
                        "Vector operations - filter - non existent",
                        """{
                               predef.filter(Int, [0,1], fun ( e : Int) predef.Ints.gt(e, 1))
                            }""",
                        "[]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - filter - even",
                        """{
                               predef.filter(Int, [0,1,2,3,4,5,6,7,8,9,10], fun ( e : Int) predef.equal(Int, predef.Ints.mod(e, 2), 0))
                            }""",
                        "[0,2,4,6,8,10]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - filter - odd",
                        """{
                               predef.filter(Int, [0,1,2,3,4,5,6,7,8,9,10], fun ( e : Int) predef.equal(Int, predef.Ints.mod(e, 2), 1))
                            }""",
                        "[1,3,5,7,9]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - filter concat filternot equals the original vector, but not sorted",
                        """{
						        val values : Vector(Int) = [0,1,2,3,4,5,6,7,8,9,10]
						        val predicate = fun ( e : Int) predef.equal(Int, predef.Ints.mod(e, 2), 0)
						        predef.Vectors.concat(Int,
                                    predef.filter(Int, values, predicate),
								    predef.filterNot(Int, values, predicate)
								    )
                            }""",
                        "[0,2,4,6,8,10,1,3,5,7,9]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - filter concat filternot equals the original vector",
                        """{
						        val values : Vector(Int) = [0,1,2,3,4,5,6,7,8,9,10]
						        val predicate = fun ( e : Int) predef.Ints.lt(e, 5)
						        predef.Vectors.concat(Int,
                                    predef.filter(Int, values, predicate),
								    predef.filterNot(Int, values, predicate)
								    )
                            }""",
                        "[0,1,2,3,4,5,6,7,8,9,10]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - find - successful",
                        """{
						        val values : Vector(Int) = [0,1,2,3,4,5,6,7,8,9,10]
						        val predicate = fun ( e : Int) predef.Ints.gt(e, 5)
						        predef.find(Int, values, predicate)
                            }""",
                        "[6]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - find - unsuccessful",
                        """{
						        val values : Vector(Int) = [0,1,2,3,4,5,6,7,8,9,10]
						        val predicate = fun ( e : Int) predef.Ints.gt(e, 10)
						        predef.find(Int, values, predicate)
                            }""",
                        "[]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - forall - successful",
                        """{
						        val values : Vector(Int) = [0,1,2,3,4,5,6,7,8,9,10]
						        val predicate = fun ( e : Int) predef.Ints.lt(e, 11)
						        predef.forall(Int, values, predicate)
                            }""",
                        "true",
                        "Boolean"
                    ),
                    ExecTest(
                        "Vector operations - forall even - successful",
                        """{
						        val values : Vector(Int) = [-4,-2,-0,0,2,4,8]
						        val predicate =  fun ( e : Int) predef.equal(Int, predef.Ints.mod(e, 2), 0)
						        predef.forall(Int, values, predicate)
                            }""",
                        "true",
                        "Boolean"
                    ),
                    ExecTest(
                        "Vector operations - forall even - unsuccessful",
                        """{
						        val values : Vector(Int) = [-4,-2,0,1,2,4,8]
						        val predicate =  fun ( e : Int) predef.equal(Int, predef.Ints.mod(e, 2), 0)
						        predef.forall(Int, values, predicate)
                            }""",
                        "false",
                        "Boolean"
                    ),
                    ExecTest(
                        "Vector operations - indexof - successful",
                        """{
						        val values : Vector(Int) = [-4,-2,0,1,2,4,8]
						        predef.indexOf(Int, values, 0)
                            }""",
                        "2",
                        "Int"
                    ),
                    ExecTest(
                        "Vector operations - indexof vector - successful",
                        """{
						        val values : Vector(Vector(Int)) = [[1,2,3],
                                                                    [2,3,4],
                                                                    [3,4,5]]
						        predef.indexOf(Int, values, [2,3,4])
                            }""",
                        "1",
                        "Int"
                    ),
                    ExecTest(
                        "Vector operations - indexof vector - unsuccessful",
                        """{
						        val values : Vector(Vector(Int)) = [[1,2,3],
                                                                    [2,3,4],
                                                                    [3,4,5]]
						        predef.indexOf(Int, values, [4,5,6])
                            }""",
                        "-1",
                        "Int"
                    ),
                    ExecTest(
                        "Vector operations - indexof - unsuccessful",
                        """{
						        val values : Vector(Int) = [-4,-2,0,1,2,4,8]
						        predef.indexOf(Int, values, 10)
                            }""",
                        "-1",
                        "Int"
                    ),
                    ExecTest(
                        "Vector operations - exists even number - successful",
                        """{
						        val values : Vector(Int) = [-4,-2,0,1,2,4,8]
			                    val predicate =  fun ( e : Int) predef.equal(Int, predef.Ints.mod(e, 2), 0)
						        predef.exists(Int, values, predicate)
                            }""",
                        "true",
                        "Boolean"
                    ),
                    ExecTest(
                        "Vector operations - exists odd number - successful",
                        """{
						        val values : Vector(Int) = [-4,-2,0,1,2,4,8]
			                    val predicate =  fun ( e : Int) predef.equal(Int, predef.Ints.mod(e, 2), 0)
						        predef.exists(Int, values, predicate)
                            }""",
                        "true",
                        "Boolean"
                    ),
                    ExecTest(
                        "Vector operations - contains - successful",
                        """{
						        val values : Vector(Int) = [-4,-2,0,1,2,4,8]
						        predef.contains(Int, values, 0)
                            }""",
                        "true",
                        "Boolean"
                    ),
                    ExecTest(
                        "Vector operations - contains vector - successful",
                        """{
						        val values : Vector(Vector(Int)) = [[1,2,3],
                                                                    [2,3,4],
                                                                    [3,4,5]]
						        predef.contains(Int, values, [1,2,3])
                            }""",
                        "true",
                        "Boolean"
                    ),
                    ExecTest(
                        "Vector operations - contains vector - unsuccessful",
                        """{
						        val values : Vector(Vector(Int)) = [[1,2,3],
                                                                    [2,3,4],
                                                                    [3,4,5]]
						        predef.contains(Int, values, [4,5,6])
                            }""",
                        "false",
                        "Boolean"
                    ),
                    ExecTest(
                        "Vector operations - count even numbers",
                        """{
						        val values : Vector(Int) = [0,1,2,3,4,5,6,7,8,9,10]
						        val predicate =  fun ( e : Int) predef.equal(Int, predef.Ints.mod(e, 2), 0)
			                    predef.count(Int, values, predicate)
                            }""",
                        "6",
                        "Int"
                    ),
                    ExecTest(
                        "Vector operations - count odd numbers",
                        """{
						        val values : Vector(Int) = [0,1,2,3,4,5,6,7,8,9,10]
						        val predicate =  fun ( e : Int) predef.equal(Int, predef.Ints.mod(e, 2), 1)
			                    predef.count(Int, values, predicate)
                            }""",
                        "5",
                        "Int"
                    ),
                    ExecTest(
                        "Vector operations - count - empty result",
                        """{
						        val values : Vector(Int) = [0,1,2,3,4,5,6,7,8,9,10]
						        val predicate =  fun ( e : Int) predef.Ints.gt(e, 10)
			                    predef.count(Int, values, predicate)
                            }""",
                        "0",
                        "Int"
                    ),
                    ExecTest(
                        "Vector operations - dropRight - non-empty vector, n > 0",
                        """{
			                    predef.dropRight(Int, [0,1,2,3,4,5], 3)
                            }""",
                        "[0,1,2]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - dropRight - non-empty vector, n = 0",
                        """{
			                    predef.dropRight(Int, [0,1,2,3,4,5], 0)
                            }""",
                        "[0,1,2,3,4,5]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - dropRight - empty vector - n = 0",
                        """{
			                    predef.dropRight(Int, [], 0)
                            }""",
                        "[]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - dropRight - empty vector - n > 0",
                        """{
			                    predef.dropRight(Int, [], 10)
                            }""",
                        "[]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - dropRight - non-empty vector - n > vector length",
                        """{
			                    predef.dropRight(Int, [1,2], 10)
                            }""",
                        "[]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - dropRight - non-empty vector - negative n",
                        """{
			                    predef.dropRight(Int, [1,2], -1)
                            }""",
                        "[1,2]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - drop - non-empty vector, n > 0",
                        """{
			                    predef.drop(Int, [0,1,2,3,4,5], 3)
                            }""",
                        "[3,4,5]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - drop - non-empty vector, n = 0",
                        """{
			                    predef.drop(Int, [0,1,2,3,4,5], 0)
                            }""",
                        "[0,1,2,3,4,5]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - drop - empty vector - n = 0",
                        """{
			                    predef.drop(Int, [], 0)
                            }""",
                        "[]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - drop - empty vector - n > 0",
                        """{
			                    predef.drop(Int, [], 10)
                            }""",
                        "[]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - drop - non-empty vector - n > vector length",
                        """{
			                    predef.drop(Int, [1,2], 10)
                            }""",
                        "[]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - drop - non-empty vector - negative n",
                        """{
			                    predef.drop(Int, [1,2], -1)
                            }""",
                        "[1,2]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - dropWhile",
                        """{
			                    predef.dropWhile(Int, [0,1,2,3,4,5,6,7,8,9,10], fun ( e : Int) predef.Ints.lte(e, 5))
                            }""",
                        "[6,7,8,9,10]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - dropWhile - drop all",
                        """{
			                    predef.dropWhile(Int, [0,1,2,3,4,5,6,7,8,9,10], fun ( e : Int) predef.Ints.lte(e, 10))
                            }""",
                        "[]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - dropWhile - drop none",
                        """{
			                    predef.dropWhile(Int, [0,1,2,3,4,5,6,7,8,9,10], fun ( e : Int) predef.Ints.gt(e, 10))
                            }""",
                        "[0,1,2,3,4,5,6,7,8,9,10]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - dropWhile - empty vector",
                        """{
			                    predef.dropWhile(Int, [], fun ( e : Int) predef.Ints.gt(e, 10))
                            }""",
                        "[]",
                        "Vector(Int)"
                    ),
                    ExecTest(
                        "Vector operations - endswith - true 1",
                        """{
			                    predef.endsWith(Int, [1,2,3], [3])
                            }""",
                        "true",
                        "Boolean"
                    ),
                    ExecTest(
                        "Vector operations - endswith - true 2",
                        """{
			                    predef.endsWith(Int, [1,2,3], [2,3])
                            }""",
                        "true",
                        "Boolean"
                    ),
                    ExecTest(
                        "Vector operations - endswith - true 3",
                        """{
			                    predef.endsWith(Int, [1,2,3], [1,2,3])
                            }""",
                        "true",
                        "Boolean"
                    ),
                    ExecTest(
                        "Vector operations - endswith - false 1",
                        """{
			                    predef.endsWith(Int, [1,2,3], [1,3])
                            }""",
                        "false",
                        "Boolean"
                    ),
                    ExecTest(
                        "Vector operations - endswith - false 2",
                        """{
			                    predef.endsWith(Int, [1,2,3], [1,2])
                            }""",
                        "false",
                        "Boolean"
                    ),
                    ExecTest(
                        "Vector operations - endswith - false 3",
                        """{
			                    predef.endsWith(Int, [1,2,3], [1])
                            }""",
                        "false",
                        "Boolean"
                    ),
                    ExecTest(
                        "Vector operations - endswith - false 4",
                        """{
			                    predef.endsWith(Int, [1,2,3], [])
                            }""",
                        "false",
                        "Boolean"
                    ),
                    ExecTest(
                        "Vector operations - endswith - false 5",
                        """{
			                    predef.endsWith(Int, [1,2,3], [3,4])
                            }""",
                        "false",
                        "Boolean"
                    )
                )

        for (aTest <- execTests) {
            test(s"${backend.name} run: ${aTest.name}") {
                val result = runString(aTest.name, aTest.program, backend.options, backend)
                result shouldBe ""
            }
            test(s"${backend.name} run: ${aTest.name}: result") {
                val result = runString(aTest.name, aTest.program, Seq("-r") ++ backend.options, backend)
                val expectedValue = aTest.expectedCompiledResult.stripMargin
                result shouldBe s"$expectedValue\n"
            }

            backend match {
                case ref @ Backend("Reference", _, _) => ref.frontend = new ReferenceFrontend
                case graal @ _                        => graal.frontend = new TruffleFrontend(out = new PrintStream(truffleOutContent))
            }
        }

        val execTestsError =
            Vector(
                ExecTestError(
                    "Vector operations - get - out of bounds",
                    """{
                            val x : Vector(Int) = [1,2,3]
                            predef.Vectors.get(Int, x, 4)
                        }""",
                    "cooma: Index out of bounds - size: 3, index: 4"
                ),
                ExecTestError(
                    "Vector operations - get - out of bounds - negative index",
                    """{
                            val x : Vector(Int) = [1,2,3]
                            predef.Vectors.get(Int, x, -1)
                        }""",
                    "cooma: Index out of bounds - size: 3, index: -1"
                ),
                ExecTestError(
                    "Vector operations - get - out of bounds - empty vector ",
                    """{
                            val x : Vector(Int) = []
                            predef.Vectors.get(Int, x, 0)
                        }""",
                    "cooma: Index out of bounds - size: 0, index: 0"
                ),
                ExecTestError(
                    "Vector operations - put on empty vector ",
                    """{
                        val x : Vector(Int) = []
                        predef.Vectors.put(Int, x, 0, 5)
                    }""",
                    "cooma: Index out of bounds - size: 0, index: 0"
                ),
                ExecTestError(
                    "Vector operations - put on vector, index out ouf bounds ",
                    """{
                        val x : Vector(Int) = [1]
                        predef.Vectors.put(Int, x, 1, 5)
                    }""",
                    "cooma: Index out of bounds - size: 1, index: 1"
                ),
                ExecTestError(
                    "Vector operations - put on vector, index out ouf bounds (negative) ",
                    """{
                        val x : Vector(Int) = [1]
                        predef.Vectors.put(Int, x, -1, 5)
                    }""",
                    "cooma: Index out of bounds - size: 1, index: -1"
                ),
                ExecTestError(
                    "Vector operations - vector slice wrong range 1 ",
                    """{
                        val x : Vector(Int) = [1,2,3]
                        predef.Vectors.slice(Int, x, -1, 1)
                    }""",
                    "cooma: Index out of bounds for slice - elems range: [0:2], targeted range: [-1:1)"
                ),
                ExecTestError(
                    "Vector operations - vector slice wrong range 2 ",
                    """{
                        val x : Vector(Int) = [1,2,3]
                        predef.Vectors.slice(Int, x, 1, 4)
                    }""",
                    "cooma: Index out of bounds for slice - elems range: [0:2], targeted range: [1:4)"
                ),
                ExecTestError(
                    "Vector operations - vector slice wrong range 3 ",
                    """{
                        val x : Vector(Int) = [1,2,3]
                        predef.Vectors.slice(Int, x, -1, 4)
                    }""",
                    "cooma: Index out of bounds for slice - elems range: [0:2], targeted range: [-1:4)"
                ),
                ExecTestError(
                    "Vector operations - vector reduce empty collection ",
                    """{
                        predef.reduce(Int, [], fun ( l : Int, r : Int) predef.Ints.add(l, r))
                    }""",
                    "cooma: Cannot apply reduce on empty collection"
                )
            )

        for (aTest <- execTestsError) {
            test(s"${backend.name} run: ${aTest.name}") {
                val errorMessage = runString(aTest.name, aTest.program, backend.options, backend)
                errorMessage shouldBe s"${aTest.expectedErrorMessage}\n"
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
            b.toString

        case class Int1PrimTest(
            op : PrimOp,
            func : (BigInt) => BigInt
        )

        case class Int2PrimTest(
            op : PrimOp,
            func : (BigInt, BigInt) => BigInt
        )

        val int1PrimTests =
            Vector(
                Int1PrimTest(ABS, _.abs),
            )

        for (aTest <- int1PrimTests) {
            val primName = aTest.op.primName
            test(s"${backend.name} run: prim $primName") {
                forAll { (i : BigInt) =>
                    runPrimTest(s"prim $primName", s"$i", "Int", s"${aTest.func(i)}")
                }
            }
        }

        val int2PrimTests =
            Vector(
                Int2PrimTest(ADD, _ + _),
                Int2PrimTest(MUL, _ * _),
                Int2PrimTest(SUB, _ - _)
            )

        for (aTest <- int2PrimTests) {
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

        {
            val primName = "Equal"

            test(s"${backend.name} run: Int prim $primName") {
                forAll { (l : BigInt, r : BigInt) =>
                    runPrimTest(s"prim $primName", s"Int, $l, $r", "Boolean", expectedBool(l == r))
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
            val primName = "Equal"
            val func = (l : String, r : String) => Util.unescape(l) == Util.unescape(r)

            test(s"${backend.name} run: String prim $primName") {
                forAll(stringLit, stringLit) { (l : String, r : String) =>
                    whenever(isStringLit(l) && isStringLit(r)) {
                        runPrimTest(s"prim $primName", s"""String, "$l", "$r"""", "Boolean", expectedBool(func(l, r)))
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
                    OptionTest("Cooma AST print", "-C", "basic/multiArgCall", "coomaAST"),
                    OptionTest("Cooma AST print", "-C", "basic/blockVal", "coomaAST"),
                    OptionTest("Cooma AST print", "-C", "basic/blockDef", "coomaAST"),
                    OptionTest("Type print", "-t", "basic/boolean", "type"),
                    OptionTest("Type print", "-t", "capability/readerCmdArg", "type", Seq("/dev/null")),
                    OptionTest("Usage", "--usage", "basic/integer", "usage", Seq()),
                    OptionTest("Usage", "--usage", "capability/readerCmdArg", "usage", Seq()),
                    OptionTest("Cooma AST print", "-C", "capability/writerCmdArg", "coomaAST", Seq("/dev/null")),
                )

            for (aTest <- optionTests) {
                val inputFilename = s"${aTest.inputBasename}.cooma"
                val expectedFilename = s"${aTest.inputBasename}.${aTest.expectedExtension}"
                val execute = backend.name == "Reference" && aTest.expectedExtension == "coomaAST"
                if (execute)
                    filetest(s"${backend.name} file", resourcesPath, s"$resourcesPath/$expectedFilename",
                        backend.options ++ List(aTest.option, s"$resourcesPath/$inputFilename") ++ aTest.args,
                        expectedFilename)
            }
        }

        // REPL tests

        for (aTest <- execTests) {
            test(s"${backend.name} REPL: ${aTest.name}") {
                val result = runREPLOnLine(aTest.program, backend.options)
                val expectedType = aTest.expectedREPLType.stripMargin
                val expectedValue = aTest.expectedCompiledResult.stripMargin
                val expectedResult = s"${aTest.expectedREPLVar} : $expectedType = $expectedValue\n"
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
                    "single evaluation (function)",
                    """
                        fun (x : Int) x
                        res0(10)
                    """,
                    "res0 : (x : Int) Int = <function>\nres1 : Int = 10"
                ),
                REPLTest(
                    "single evaluation (function using type alias)",
                    """
                        fun (x : Boolean) x
                        res0(true)
                    """,
                    "res0 : (x : Boolean) Boolean = <function>\nres1 : Boolean = true"
                ),
                REPLTest(
                    "single evaluation (function using type alias that needs to be expanded)",
                    """
                        fun (x : Reader) x.read()
                        res0({read = fun () "hello"})
                    """,
                    "res0 : (x : Reader) String = <function>\nres1 : String = \"hello\""
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
                    "single value definition, implicit type (simple)",
                    """
                        val x = 1
                        x
                    """,
                    "x : Int = 1\nx : Int = 1"
                ),
                REPLTest(
                    "single value definition, explicit type (simple)",
                    """
                        val x : Int = 1
                        x
                    """,
                    "x : Int = 1\nx : Int = 1"
                ),
                REPLTest(
                    "single value definition (complex, no type arg)",
                    """
                        val id = fun (x : Int) x
                        id(3)
                    """,
                    "id : (x : Int) Int = <function>\nres0 : Int = 3"
                ),
                REPLTest(
                    "single value definition (complex, type arg)",
                    """
                        val id = fun (t : Type, x : t) x
                        id(Int, 3)
                    """,
                    "id : (t : Type, x : t) t = <function>\nres0 : Int = 3"
                ),
                REPLTest(
                    "single value definition (nested type alias)",
                    "{ type X = Int val x : X = 3 x }",
                    "res0 : Int = 3"
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
                    "f : (x : Int) Int = <function>\nres0 : Int = 10"
                ),
                REPLTest(
                    "value and function definition",
                    """
                        val x = 10
                        def f(y : Int) Int = x
                        f(20)
                    """,
                    "x : Int = 10\nf : (y : Int) Int = <function>\nres0 : Int = 10"
                ),
                REPLTest(
                    "multiple function definitions (upper)",
                    """
                        def f(x : Int) Int = 10
                        def g(y : Int) Int = 20
                        f(1)
                    """,
                    "f : (x : Int) Int = <function>\ng : (y : Int) Int = <function>\nres0 : Int = 10"
                ),
                REPLTest(
                    "multiple function definitions (lower)",
                    """
                        def f(x : Int) Int = 10
                        def g(y : Int) Int = 20
                        g(1)
                    """,
                    "f : (x : Int) Int = <function>\ng : (y : Int) Int = <function>\nres0 : Int = 20"
                ),
                REPLTest(
                    "multiple function definitions (chain)",
                    """
                        def f(x : Int) Int = 10
                        def g(y : Int) Int = f(y)
                        g(1)
                    """,
                    "f : (x : Int) Int = <function>\ng : (y : Int) Int = <function>\nres0 : Int = 10"
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
                    "built-in Int type",
                    "Int",
                    "Int : Type = Int"
                ),
                REPLTest(
                    "built-in Boolean type",
                    "Boolean",
                    "Boolean : Type = < False : Unit, True : Unit >"
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
        repl.enterline(Predef.predefREPL, config)
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
