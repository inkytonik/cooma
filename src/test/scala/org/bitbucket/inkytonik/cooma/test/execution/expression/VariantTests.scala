package org.bitbucket.inkytonik.cooma.test.execution.expression

import org.bitbucket.inkytonik.cooma.test.ExpressionTests

class VariantTests extends ExpressionTests {

    test(
        "variant (single int field)",
        "<< x = 65 >>",
        "<< x = 65 >>",
        "<< x : Int >>"
    )

    test(
        "variant (single string field)",
        """<< name = "Harold" >>""",
        """<< name = "Harold" >>""",
        "<< name : String >>"
    )

    test(
        "variant (eval field)",
        "<< a = {fun (x : Int) x}(3) >>",
        "<< a = 3 >>",
        "<< a : Int >>"
    )

    test(
        "multi-line variant",
        """<<
            name = "Bob"
        >>""",
        """<< name = "Bob" >>""",
        "<< name : String >>"
    )

    test(
        "basic match",
        "<< x = 1 >> match { case x(a) => a }",
        "1",
        "Int"
    )

    test(
        "multi-case match (first case, same order)",
        """{
            def f () << x : Int, y : Int >> = << x = 3 >>
            f () match { case x(a) => 1 case y(b) => 2 }
        }""",
        "1",
        "Int"
    )

    test(
        "multi-case match (later case, same order)",
        """{
            def f () << x : Int, y : Int >> = << y = 3 >>
            f () match { case x(a) => 1 case y(b) => 2 }
        }""",
        "2",
        "Int"
    )

    test(
        "multi-case match (first case, different order)",
        """{
            def f () << x : Int, y : Int >> = << y = 3 >>
            f () match { case y(b) => 1 case x(a) => 2 }
        }""",
        "1",
        "Int"
    )

    test(
        "multi-case match (later case, different order)",
        """{
            def f () << x : Int, y : Int >> = << x = 3 >>
            f () match { case y(b) => 1 case x(a) => 2 }
        }""",
        "2",
        "Int"
    )

    test(
        "multi-case match (different bound var types, same order, first case)",
        """{
            def f () << x : Int, y : String >> = << x = 3 >>
            f () match { case x(a) => 1 case y(b) => prim StrLength(b) }
        }""",
        "1",
        "Int"
    )

    test(
        "multi-case match (different bound var types, same order, second case)",
        """{
            def f () << x : Int, y : String >> = << y = "hi" >>
            f () match { case x(a) => 1 case y(b) => prim StrLength(b) }
        }""",
        "2",
        "Int"
    )

    test(
        "multi-case match (different bound var types, different order, first case)",
        """{
            def f () << x : Int, y : String >> = << y = "hi" >>
            f () match { case y(b) => prim StrLength(b) case x(a) => 1 }
        }""",
        "2",
        "Int"
    )

    test(
        "multi-case match (different bound var types, different order, second case)",
        """{
            def f () << x : Int, y : String >> = << x = 3 >>
            f () match { case y(b) => prim StrLength(b) case x(a) => 1 }
        }""",
        "1",
        "Int"
    )

    test(
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
    )

}
