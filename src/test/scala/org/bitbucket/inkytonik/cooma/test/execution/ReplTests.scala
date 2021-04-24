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
        "res0 : (x : Boolean) Boolean = <function>\nres1 : Boolean = true"
    )

    test(
        "single evaluation (function using type alias that needs to be expanded)",
        """
                        fun (x : Reader) x.read()
                        res0({read = fun () "hello"})
                    """,
        "res0 : (x : Reader) String = <function>\nres1 : String = \"hello\""
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
        "built-in Boolean type",
        "Boolean",
        "Boolean : Type = < False : Unit, True : Unit >"
    )

}
