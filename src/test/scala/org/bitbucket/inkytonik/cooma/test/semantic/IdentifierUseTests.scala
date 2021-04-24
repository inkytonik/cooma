package org.bitbucket.inkytonik.cooma.test.semantic

import org.bitbucket.inkytonik.cooma.test.SemanticTests

class IdentifierUseTests extends SemanticTests {

    test(
        "Int can't be used as user identifier",
        "{ val Int = 0 1 }",
        """|1:7:error: Int is a reserved primitive type name
           |{ val Int = 0 1 }
           |      ^
           |"""
    )

    test(
        "String can't be used as user identifier",
        "{ val String = 0 1 }",
        """|1:7:error: String is a reserved primitive type name
           |{ val String = 0 1 }
           |      ^
           |"""
    )

    test(
        "Type can't be used as user identifier",
        "{ val Type = 0 1 }",
        """|1:7:error: Type is a reserved primitive type name
           |{ val Type = 0 1 }
           |      ^
           |"""
    )

    test(
        "Unit can't be used as user identifier",
        "{ val Unit = 0 1 }",
        """|1:7:error: Unit is a reserved primitive type name
           |{ val Unit = 0 1 }
           |      ^
           |"""
    )

    test(
        "val not usable in its own type",
        "{ val x : x = 0 1 }",
        """|1:11:error: x is not declared
           |{ val x : x = 0 1 }
           |          ^
           |"""
    )

    test(
        "val not usable in its own definition",
        "{ val x = x 1 }",
        """|1:11:error: x is not declared
           |{ val x = x 1 }
           |          ^
           |"""
    )

    test(
        "underscore val not usable",
        "{ val _ = 3 _ }",
        """|1:13:error: _ is not declared
           |{ val _ = 3 _ }
           |            ^
           |"""
    )

    test(
        "underscore argument not usable (fun)",
        "{fun (_ : Int) _}(0)",
        """|1:16:error: _ is not declared
           |{fun (_ : Int) _}(0)
           |               ^
           |"""
    )

    test(
        "underscore argument not usable (def)",
        "{ def f (_ : Int) Int = _ 0 }",
        """|1:25:error: _ is not declared
           |{ def f (_ : Int) Int = _ 0 }
           |                        ^
           |"""
    )

    test(
        "underscore def not usable",
        "{ def _ (x : Int) Int = 0 _(1) }",
        """|1:27:error: _ is not declared
           |{ def _ (x : Int) Int = 0 _(1) }
           |                          ^
           |"""
    )

    test(
        "lone name",
        "x",
        """|1:1:error: x is not declared
           |x
           |^
           |"""
    )

    test(
        "non-declared name in field definition (record first)",
        "{ x = y }",
        """|1:7:error: y is not declared
           |{ x = y }
           |      ^
           |"""
    )

    test(
        "non-declared name in field definition (record second)",
        "{ x = 1, y = z }",
        """|1:14:error: z is not declared
           |{ x = 1, y = z }
           |             ^
           |"""
    )

    test(
        "non-declared name in field definition (variant)",
        "< x = y >",
        """|1:7:error: y is not declared
           |< x = y >
           |      ^
           |"""
    )

    test(
        "declared value name",
        "{ val x = 1 x}",
        ""
    )

    test(
        "redeclared value name",
        "{ val x = 1 val x = 2 x}",
        ""
    )

    test(
        "not-declared value name",
        "{ val x = 1 y}",
        """|1:13:error: y is not declared
           |{ val x = 1 y}
           |            ^
           |"""
    )

    test(
        "declared argument name",
        "{fun (x : Int) x}(0)",
        ""
    )

    test(
        "argument name not in argument type scope",
        "{fun (x : x) 0}(0)",
        """|1:11:error: x is not declared
           |{fun (x : x) 0}(0)
           |          ^
           |"""
    )

    test(
        "not-declared use in no argument function",
        "{fun () x}()",
        """|1:9:error: x is not declared
           |{fun () x}()
           |        ^
           |"""
    )

    test(
        "not-declared use in argument function",
        "{fun (x : Int) y}(0)",
        """|1:16:error: y is not declared
           |{fun (x : Int) y}(0)
           |               ^
           |"""
    )

    test(
        "not-declared use in nested expression",
        "{fun (x : { a : Int }) x & y}({a = 1})",
        """|1:28:error: y is not declared
           |{fun (x : { a : Int }) x & y}({a = 1})
           |                           ^
           |"""
    )

    test(
        "not-declared use in field definition",
        "fun () { a = y }",
        """|1:14:error: y is not declared
           |fun () { a = y }
           |             ^
           |"""
    )

    test(
        "not-declared use in value definition (self)",
        "{ val x = x 0 }",
        """|1:11:error: x is not declared
           |{ val x = x 0 }
           |          ^
           |"""
    )

    test(
        "not-declared use in value definition (other)",
        "{ val x = y 0 }",
        """|1:11:error: y is not declared
           |{ val x = y 0 }
           |          ^
           |"""
    )

    test(
        "overriding value names",
        """{ val x = "hi" val x = 2 {fun (a : Int) 0}(x) }""",
        ""
    )

    test(
        "not-declared use in no argument function definition",
        "{ def f () Int = x 0 }",
        """|1:18:error: x is not declared
           |{ def f () Int = x 0 }
           |                 ^
           |"""
    )

    test(
        "not-declared use in argument function definition",
        "{ def f (x : Int) Int = y 0 }",
        """|1:25:error: y is not declared
           |{ def f (x : Int) Int = y 0 }
           |                        ^
           |"""
    )

    test(
        "not-declared use as type name in function",
        "{fun (x : Foo) 0}(0)",
        """|1:11:error: Foo is not declared
           |{fun (x : Foo) 0}(0)
           |          ^
           |"""
    )

    test(
        "not-declared use as type name in function defintion",
        "{ def f (x : Foo) Int = 0 0 }",
        """|1:14:error: Foo is not declared
           |{ def f (x : Foo) Int = 0 0 }
           |             ^
           |"""
    )

    test(
        "overriding function names in different group",
        "{ def f (i : String) Int = 0 val x = 1 def f (i : Int) Int = i f(0) }",
        ""
    )

    test(
        "ok self reference function in same group",
        "{ def f(x : Int) Int = f(x) f(10) }",
        ""
    )

    test(
        "def use (far, one group)",
        "{ def f(x : Int) Int = x def g(x : Int) Int = x f(10) }",
        ""
    )

    test(
        "def use (near, one group)",
        "{ def f(x : Int) Int = x def g(x : Int) Int = x g(10) }",
        ""
    )

    test(
        "def use (far, two groups)",
        "{ def f(x : Int) Int = x val x = 1 def g(x : Int) Int = x f(10) }",
        ""
    )

    test(
        "ok forward reference function in same group",
        "{ def f(x : Int) Int = g(x) def g(x : Int) Int = x f(10) }",
        ""
    )

    test(
        "bad forward reference function to different group",
        "{ def f(x : Int) Int = g(x) val x = 1 def g(x : Int) Int = x f(10) }",
        """|1:24:error: g is not declared
           |{ def f(x : Int) Int = g(x) val x = 1 def g(x : Int) Int = x f(10) }
           |                       ^
           |"""
    )

    test(
        "ok backward reference function in same group",
        "{ def f(x : Int) Int = x def g(x : Int) Int = f(x) g(10) }",
        ""
    )

    test(
        "ok backward reference function to different group",
        "{ def f(x : Int) Int = x val x = 1 def g(x : Int) Int = f(x) g(10) }",
        ""
    )

}
