package org.bitbucket.inkytonik.cooma.test.semantic

import org.bitbucket.inkytonik.cooma.test.SemanticTests

class DeclarationTests extends SemanticTests {

    test(
        "illegal main program argument type (single)",
        "fun (x : Int) x",
        """|1:10:error: illegal main program argument type
           |fun (x : Int) x
           |         ^
           |"""
    )

    test(
        "illegal main program argument type (multiple)",
        "fun (x : Boolean, y : Int) x",
        """|1:10:error: illegal main program argument type
           |fun (x : Boolean, y : Int) x
           |         ^
           |1:23:error: illegal main program argument type
           |fun (x : Boolean, y : Int) x
           |                      ^
           |"""
    )

    test(
        "ok main program argument type (indirect)",
        "{fun (x : Int) fun (_ : Int) x}(0)",
        ""
    )

    test(
        "distinct argument names (fun)",
        "{fun (x : Int, y : Int) x}(0, 0)",
        ""
    )

    test(
        "duplicated underscore argument names (fun)",
        "{fun (_ : Int, _ : Int) 3}(0, 0)",
        ""
    )

    test(
        "duplicated argument name (fun)",
        "{fun (x : Int, x : Int) x}(0, 0)",
        """|1:16:error: re-declaration of x
           |{fun (x : Int, x : Int) x}(0, 0)
           |               ^
           |"""
    )

    test(
        "duplicated variant name in variant type",
        "{fun (a : << x : Int, x : Int, y : Int >>) a}(<<x = 3>>)",
        """|1:14:error: duplicate type field x
           |{fun (a : << x : Int, x : Int, y : Int >>) a}(<<x = 3>>)
           |             ^
           |1:23:error: duplicate type field x
           |{fun (a : << x : Int, x : Int, y : Int >>) a}(<<x = 3>>)
           |                      ^
           |"""
    )

    test(
        "underscore val",
        "{ val _ = 3 0 }",
        ""
    )

    test(
        "distinct function names",
        "{ def f (i : Int) Int = i def g (i : Int) Int = i 0 }",
        ""
    )

    test(
        "duplicate function names in same group",
        "{ def f (i : Int) Int = i def f (i : Int) Int = i 0 }",
        """|1:31:error: re-declaration of f
           |{ def f (i : Int) Int = i def f (i : Int) Int = i 0 }
           |                              ^
           |"""
    )

    test(
        "distinct argument names (def)",
        "{ def f (x : Int, y : Int) Int = x 0 }",
        ""
    )

    test(
        "duplicated underscore argument names (def)",
        "{ def f (_ : Int, _ : Int) Int = 0 0 }",
        ""
    )

    test(
        "duplicated argument names (def)",
        "{ def f (x : Int, x : Int) Int = x 0 }",
        """|1:19:error: re-declaration of x
           |{ def f (x : Int, x : Int) Int = x 0 }
           |                  ^
           |"""
    )

    test(
        "val explicit type must be a type",
        "{ val x : 7 = 1 x }",
        """|1:11:error: expected Type, got 7 of type Int
           |{ val x : 7 = 1 x }
           |          ^
           |1:15:error: expected 7, got 1 of type Int
           |{ val x : 7 = 1 x }
           |              ^
           |"""
    )

    test(
        "val explicit simple type (ok)",
        "{ val x : Int = 1 x }",
        ""
    )

    test(
        "val explicit simple type (bad)",
        "{ val x : String = 1 x }",
        """|1:20:error: expected String, got 1 of type Int
           |{ val x : String = 1 x }
           |                   ^
           |"""
    )

    test(
        "val explicit function type (ok)",
        "{ val f : (Int) Int = fun (x : Int) x f(0)}",
        ""
    )

    test(
        "val explicit function type (bad)",
        """{ val f : (String) Int = fun (x : Int) x f("hi")}""",
        """|1:26:error: expected (String) Int, got fun (x : Int) x of type (x : Int) Int
           |{ val f : (String) Int = fun (x : Int) x f("hi")}
           |                         ^
           |"""
    )

    test(
        "val explicit record type (ok)",
        "{ val x : { a : Int, b : Int } = { a = 1, b = 2 } x }",
        ""
    )

    test(
        "val explicit record type (bad)",
        "{ val x : { a : Int, b : String } = { a = 1 } x }",
        """|1:37:error: expected { a : Int, b : String }, got { a = 1 } of type { a : Int }
           |{ val x : { a : Int, b : String } = { a = 1 } x }
           |                                    ^
           |"""
    )

    test(
        "val explicit variant type (ok)",
        "{ val x : << a : Int, b : String >> = << a = 1 >> x }",
        ""
    )

    test(
        "val explicit variant type (bad)",
        "{ val x : << a : Int, b : String >> = << c = 1 >> x }",
        """|1:39:error: expected << a : Int, b : String >>, got << c = 1 >> of type << c : Int >>
           |{ val x : << a : Int, b : String >> = << c = 1 >> x }
           |                                      ^
           |"""
    )

    test(
        "val explicit pre=defined type (ok)",
        "{ val x : Boolean = << True = {} >> x }",
        ""
    )

    test(
        "val explicit pre=defined type (bad)",
        "{ val x : Boolean = 1 x }",
        """|1:21:error: expected << False : Unit, True : Unit >>, got 1 of type Int
           |{ val x : Boolean = 1 x }
           |                    ^
           |"""
    )

}
