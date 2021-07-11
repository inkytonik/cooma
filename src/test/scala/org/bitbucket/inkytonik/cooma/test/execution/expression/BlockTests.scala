package org.bitbucket.inkytonik.cooma.test.execution.expression

import org.bitbucket.inkytonik.cooma.test.ExpressionTests

class BlockTests extends ExpressionTests {

    test(
        "trivial block",
        "{ 10 }",
        "10",
        "Int"
    )

    test(
        "val block",
        """{
           val x = 10
           x
        }""",
        "10",
        "Int"
    )

    test(
        "val block with redefinition",
        """{
           val x = 10
           val x = 20
           x
        }""",
        "20",
        "Int"
    )

    test(
        "val block (inner ref)",
        """{
            val x = 10
            val y = 20
            y
        }""",
        "20",
        "Int"
    )

    test(
        "val block (outer ref)",
        """{
            val x = 10
            val y = 20
            x
        }""",
        "10",
        "Int"
    )

    test(
        "val block with functions",
        """{
            val f = fun (x : Int) x
            val g = fun (y : Int) f(y)
            g(10)
        }""",
        "10",
        "Int"
    )

    test(
        "def block no arguments",
        """{
            def f() Int = 10
            f()
        }""",
        "10",
        "Int"
    )

    test(
        "def block (single)",
        """{
            def f(x : Int) Int = x
            f(10)
        }""",
        "10",
        "Int"
    )

    test(
        "def block (multi inner)",
        """{
            def f(x : Int) Int = x
            def g(y : Int) Int = f(y)
            g(10)
        }""",
        "10",
        "Int"
    )

    test(
        "def block (multi outer)",
        """{
            def f(x : Int) Int = x
            def g(y : Int) Int = f(y)
            f(10)
        }""",
        "10",
        "Int"
    )

    test(
        "block (val and def)",
        """{
            val a = 20
            def f(x : Int) Int = a
            f(10)
        }""",
        "20",
        "Int"
    )

    test(
        "def block (self reference, tail call)",
        """{
            def f(x : Int) Int =
                equal(Int, x, 0) match {
                    case True(_)  => 20
                    case False(_) => f(Ints.sub(x, 1))
                }
            f(10)
        }""",
        "20",
        "Int"
    )

    test(
        "def block (accumulator, tail call)",
        """{
            def f(s : Int, x : Int) Int =
                equal(Int, x, 0) match {
                    case True(_)  => s
                    case False(_) => f(Ints.add(s, x), Ints.sub(x, 1))
                }
            f(0, 10)
        }""",
        "55",
        "Int"
    )

    test(
        "def block (multi forward reference)",
        """{
            def f(x : Int) Int = g(x)
            def g(y : Int) Int = y
            f(10)
        }""",
        "10",
        "Int"
    )

    test(
        "def redefinition",
        """{
            def f(x : Int) Int = 10
            val a = 20
            def f(y : Int) Int = 30
            f(0)
        }""",
        "30",
        "Int"
    )

    test(
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
    )

    test(
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
    )

    test(
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
    )

    test(
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
    )

    test(
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
    )

    test(
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
    )

    test(
        "nested def block (env capture)",
        """{
            val a = 1
            val f = {
                val a = 2
                def f(x : Int) Int = prim IntAdd(x, a)
                f
            }
            f(10)
        }""",
        "12",
        "Int"
    )

    test(
        "variant argument",
        """{
            def f (r : << x : Int, y : Int, z : Int >>) << x : Int, y : Int, z : Int >> = r
            def g () << x : Int, y : Int >> = << x = 3 >>
            f(g())
            }""",
        "<< x = 3 >>",
        "<< x : Int, y : Int, z : Int >>"
    )

}
