package org.bitbucket.inkytonik.cooma.test.execution

import org.bitbucket.inkytonik.cooma.test.ExecutionTests
import org.bitbucket.inkytonik.kiama.util.StringConsole

class ReplTests extends ExecutionTests {

    def test(name : String, program : String, expectedResult : String) : Unit =
        super.test(s"REPL: $name") { implicit bc =>
            val allArgs = Seq("--Koutput", "string") ++ bc.options
            val config = createConfig(allArgs)
            val replInput =
                if (program.indexOf('\n') == -1) program
                else s":lines\n$program\n:end"
            val console = new StringConsole(replInput)
            val repl = createREPL(config)
            val result = runTest(repl.processconsole(console, "dummy", _), allArgs)
            result shouldBe s"$expectedResult\n"
        }

    test(
        "single evaluation (int)",
        """
                        10
                    """,
        "res0 : Int = 10"
    )

    test(
        "single evaluation (string)",
        """
                        "Hello"
                    """,
        """res0 : String = "Hello""""
    )

    test(
        "single evaluation (function)",
        """
                        fun (x : Int) x
                        res0(10)
                    """,
        "res0 : (x : Int) Int = <function>\nres1 : Int = 10"
    )

    test(
        "single evaluation (function using type alias)",
        """
                        fun (x : Boolean) x
                        res0(true)
                    """,
        "res0 : (x : < False : Unit, True : Unit >) < False : Unit, True : Unit > = <function>\nres1 : < False : Unit, True : Unit > = < True = {} >"
    )

    test(
        "single evaluation (function using type alias that needs to be expanded)",
        """
                        fun (x : Reader) x.read()
                        res0({read = fun () <Right = "hello">})
                    """,
        """|res0 : (x : {
           |  read : () <
           |    Left : String,
           |    Right : String
           |  >
           |}) <
           |  Left : String,
           |  Right : String
           |> = <function>
           |res1 : < Left : String, Right : String > = < Right = "hello" >""".stripMargin
    )

    test(
        "multiple evaluations",
        """
                        10
                        20
                    """,
        "res0 : Int = 10\nres1 : Int = 20"
    )

    test(
        "single value definition, implicit type (simple)",
        """
                        val x = 1
                        x
                    """,
        "x : Int = 1\nx : Int = 1"
    )

    test(
        "single value definition, explicit type (simple)",
        """
                        val x : Int = 1
                        x
                    """,
        "x : Int = 1\nx : Int = 1"
    )

    test(
        "single value definition (complex, no type arg)",
        """
                        val id = fun (x : Int) x
                        id(3)
                    """,
        "id : (x : Int) Int = <function>\nres0 : Int = 3"
    )

    test(
        "single value definition (complex, type arg)",
        """
                        val id = fun (t : Type, x : t) x
                        id(Int, 3)
                    """,
        "id : (t : Type, x : t) t = <function>\nres0 : Int = 3"
    )

    test(
        "single value definition (nested type alias)",
        "{ type X = Int val x : X = 3 x }",
        "res0 : Int = 3"
    )

    test(
        "multiple value definitions (upper)",
        """
                        val x = 1
                        val y = 2
                        x
                    """,
        "x : Int = 1\ny : Int = 2\nx : Int = 1"
    )

    test(
        "multiple value definitions (lower)",
        """
                        val x = 1
                        val y = 2
                        y
                    """,
        "x : Int = 1\ny : Int = 2\ny : Int = 2"
    )

    test(
        "multiple value definitions (redefinition)",
        """
                        val x = 1
                        val x = 2
                        x
                    """,
        "x : Int = 1\nx : Int = 2\nx : Int = 2"
    )

    test(
        "single function definition",
        """
                        def f(x : Int) Int = x
                        f(10)
                    """,
        "f : (x : Int) Int = <function>\nres0 : Int = 10"
    )

    test(
        "value and function definition",
        """
                        val x = 10
                        def f(y : Int) Int = x
                        f(20)
                    """,
        "x : Int = 10\nf : (y : Int) Int = <function>\nres0 : Int = 10"
    )

    test(
        "multiple function definitions (upper)",
        """
                        def f(x : Int) Int = 10
                        def g(y : Int) Int = 20
                        f(1)
                    """,
        "f : (x : Int) Int = <function>\ng : (y : Int) Int = <function>\nres0 : Int = 10"
    )

    test(
        "multiple function definitions (lower)",
        """
                        def f(x : Int) Int = 10
                        def g(y : Int) Int = 20
                        g(1)
                    """,
        "f : (x : Int) Int = <function>\ng : (y : Int) Int = <function>\nres0 : Int = 20"
    )

    test(
        "multiple function definitions (chain)",
        """
                        def f(x : Int) Int = 10
                        def g(y : Int) Int = f(y)
                        g(1)
                    """,
        "f : (x : Int) Int = <function>\ng : (y : Int) Int = <function>\nres0 : Int = 10"
    )

    test(
        "single result name binding from constant",
        """
                        10
                        res0
                    """,
        "res0 : Int = 10\nres0 : Int = 10"
    )

    test(
        "single result name binding from val",
        """
                        val x = 10
						x
                    """,
        "x : Int = 10\nx : Int = 10"
    )

    test(
        "multiple result name binding",
        """
                        10
                        20
                        30
                        res2
                    """,
        "res0 : Int = 10\nres1 : Int = 20\nres2 : Int = 30\nres2 : Int = 30"
    )

    test(
        "built-in Int type",
        "Int",
        "Int : Type = Int"
    )

    test(
        "prelude Boolean type",
        "Boolean",
        "Boolean : Type = < False : Unit, True : Unit >"
    )

    test(
        "user-defined type alias for built-in type (direct, inferred type)",
        "type Foo = Int",
        "Foo : Type = Int"
    )

    test(
        "user-defined type alias for built-in type (direct, given type)",
        "type Foo : Type = Int",
        "Foo : Type = Int"
    )

    test(
        "user-defined type alias for built-in type (block)",
        "{ type Foo = Int Foo }",
        "res0 : Type = { type Foo = Int Foo }"
    )

    test(
        "user-defined type alias for prelude type (multiple)",
        """
           type MyBool = Boolean
           val b : MyBool = true
           b
        """,
        "MyBool : Type = < False : Unit, True : Unit >\nb : MyBool = < True = {} >\nb : < False : Unit, True : Unit > = < True = {} >"
    )

    test(
        "REPL-defined type alias for prelude type constructor (multiple)",
        """
           Option(Int)
           res0
        """,
        "res0 : Type = < None : Unit, Some : Int >\nres0 : Type = < None : Unit, Some : Int >"
    )

    test(
        "user-defined type alias for prelude type constructor (multiple)",
        """
           type IntOption = Option(Int)
           val o : IntOption = < Some = 42 >
           o
        """,
        "IntOption : Type = < None : Unit, Some : Int >\no : IntOption = < Some = 42 >\no : < None : Unit, Some : Int > = < Some = 42 >"
    )

    test(
        "user-defined type alias for prelude type (block)",
        "{ type MyBool = Boolean val b : MyBool = true b }",
        "res0 : < False : Unit, True : Unit > = < True = {} >"
    )

    test(
        "user-defined type alias for prelude type constructor (block)",
        "{ type IntOption = Option(Int) val o : IntOption = < Some = 42 > o }",
        "res0 : < None : Unit, Some : Int > = < Some = 42 >"
    )

    test(
        "user-defined type alias for structured type (direct, inferred type)",
        "type Foo = { foo : Int}",
        "Foo : Type = { foo : Int }"
    )

    test(
        "user-defined type alias for structured type (direct, given type)",
        "type Foo : Type = { foo : Int}",
        "Foo : Type = { foo : Int }"
    )

    test(
        "user-defined type alias for structured type (block)",
        "{ type Foo = { foo : Int} Foo }",
        "res0 : Type = { type Foo = { foo : Int } Foo }"
    )

}
