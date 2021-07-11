package org.bitbucket.inkytonik.cooma.test.semantic

import org.bitbucket.inkytonik.cooma.test.SemanticTests

class FunctionTypeTests extends SemanticTests {

    test(
        "apply no-arg function",
        "{fun () 0}()",
        ""
    )

    test(
        "apply function to too many arguments (zero)",
        "{fun () 0}(3)",
        """|1:12:error: expected no arguments, got 1
           |{fun () 0}(3)
           |           ^
           |"""
    )

    test(
        "apply function to too many arguments (one)",
        "{fun (x : Int) 0}(2, 3)",
        """|1:19:error: expected up to one argument, got 2
           |{fun (x : Int) 0}(2, 3)
           |                  ^
           |"""
    )

    test(
        "apply function to too many arguments (more than one)",
        "{fun (x : Int, y : Int) 0}(2, 3, 4)",
        """|1:28:error: expected up to 2 arguments, got 3
           |{fun (x : Int, y : Int) 0}(2, 3, 4)
           |                           ^
           |"""
    )

    test(
        "application of non-function",
        "4(2, 3)",
        """|1:1:error: application of non-function type Int
           |4(2, 3)
           |^
           |"""
    )

    test(
        "type argument",
        "{fun (t : Type, x : t) x}",
        ""
    )

    test(
        "type application",
        "{fun (t : Type, x : t) x}(Int)",
        ""
    )

    test(
        "type application (one parameterised arg)",
        "{fun (t : Type, x : t) x}(Int, 1)",
        ""
    )

    test(
        "type application (two parameterised args)",
        "{fun (t : Type, x : t, y : t) x}(Int, 1, 2)",
        ""
    )

    test(
        "type application (one parameterised arg with normal arg)",
        """{fun (t : Type, x : t, s : String) x}(Int, 1, "hi")""",
        ""
    )

    test(
        "bad type application (mis-matched usage of parameterised arg)",
        """{fun (t : Type, x : t) x}(Int, "hi")""",
        """|1:32:error: expected Int, got "hi" of type String
           |{fun (t : Type, x : t) x}(Int, "hi")
           |                               ^
           |"""
    )

    test(
        "bad type application (mis-matched usage of normal arg)",
        """{fun (t : Type, x : t, s : String) x}(Int, 1, 2)""",
        """|1:47:error: expected String, got 2 of type Int
           |{fun (t : Type, x : t, s : String) x}(Int, 1, 2)
           |                                              ^
           |"""
    )

    test(
        "bad type application (values as types and vice versa)",
        "{fun (x : Int, t : Type) x}(Int, 10)",
        """|1:29:error: expected Int, got Int of type Type
           |{fun (x : Int, t : Type) x}(Int, 10)
           |                            ^
           |1:34:error: expected Type, got 10 of type Int
           |{fun (x : Int, t : Type) x}(Int, 10)
           |                                 ^
           |"""
    )

    test(
        "function argument type same (one)",
        "{fun (x : Int) x}(1)",
        ""
    )

    test(
        "function argument type same (two)",
        "{fun (x : Int, y : Int) x}(1, 2)",
        ""
    )

    test(
        "function definition argument type same (one)",
        "{ def f (x : Int) Int = x f(1) }",
        ""
    )

    test(
        "function definition argument type same (two)",
        "{ def f (x : Int, y : Int) Int = x f(1, 2) }",
        ""
    )

    test(
        "function argument type that isn't a type",
        "{fun (x : 3) x}(1)",
        """|1:11:error: expected Type, got 3 of type Int
           |{fun (x : 3) x}(1)
           |          ^
           |1:17:error: expected 3, got 1 of type Int
           |{fun (x : 3) x}(1)
           |                ^
           |"""
    )

    test(
        "bad function argument type (one, simple)",
        "{fun (x : String) x}(1)",
        """|1:22:error: expected String, got 1 of type Int
           |{fun (x : String) x}(1)
           |                     ^
           |"""
    )

    test(
        "bad function argument type (two, simple)",
        "{fun (x : Int, y : String) x}(1, 2)",
        """|1:34:error: expected String, got 2 of type Int
           |{fun (x : Int, y : String) x}(1, 2)
           |                                 ^
           |"""
    )

    test(
        "bad function argument type (record formal)",
        "{fun (x : { y : Int }) x}(1)",
        """|1:27:error: expected { y : Int }, got 1 of type Int
           |{fun (x : { y : Int }) x}(1)
           |                          ^
           |"""
    )

    test(
        "bad function argument type (record actual)",
        "{fun (x : Int) x}({ y = 1 })",
        """|1:19:error: expected Int, got { y = 1 } of type { y : Int }
           |{fun (x : Int) x}({ y = 1 })
           |                  ^
           |"""
    )

    test(
        "bad function argument type (function formal)",
        "{fun (x : (Int) String) x}(1)",
        """|1:28:error: expected (Int) String, got 1 of type Int
           |{fun (x : (Int) String) x}(1)
           |                           ^
           |"""
    )

    test(
        "bad function argument type (function actual)",
        "{fun (x : Int) x}(fun (y : Int) y)",
        """|1:19:error: expected Int, got fun (y : Int) y of type (y : Int) Int
           |{fun (x : Int) x}(fun (y : Int) y)
           |                  ^
           |"""
    )

    test(
        "bad function definition argument type (one, simple)",
        "{ def f (x : String) Int = 0 f(1) }",
        """|1:32:error: expected String, got 1 of type Int
           |{ def f (x : String) Int = 0 f(1) }
           |                               ^
           |"""
    )

    test(
        "bad function definition argument type (two, simple)",
        "{ def f (x : Int, y : String) Int = x f(1, 2) }",
        """|1:44:error: expected String, got 2 of type Int
           |{ def f (x : Int, y : String) Int = x f(1, 2) }
           |                                           ^
           |"""
    )

    test(
        "bad function definition argument type (record formal)",
        "{ def f (x : { y : Int }) Int = 0 f(1) }",
        """|1:37:error: expected { y : Int }, got 1 of type Int
           |{ def f (x : { y : Int }) Int = 0 f(1) }
           |                                    ^
           |"""
    )

    test(
        "bad function definition argument type (record actual)",
        "{ def f (x : Int) Int = x f({ y = 1 }) }",
        """|1:29:error: expected Int, got { y = 1 } of type { y : Int }
           |{ def f (x : Int) Int = x f({ y = 1 }) }
           |                            ^
           |"""
    )

    test(
        "bad function definition argument type (function formal)",
        "{ def f (x : (Int) String) Int = 0 f(1) }",
        """|1:38:error: expected (Int) String, got 1 of type Int
           |{ def f (x : (Int) String) Int = 0 f(1) }
           |                                     ^
           |"""
    )

    test(
        "bad function definition argument type (function actual)",
        "{ def f (x : Int) Int = x f(fun (y : Int) y) }",
        """|1:29:error: expected Int, got fun (y : Int) y of type (y : Int) Int
           |{ def f (x : Int) Int = x f(fun (y : Int) y) }
           |                            ^
           |"""
    )

    test(
        "function definition return type that isn't a type",
        "{ def f (x : Int) 1 = { x = 1 } f(0) }",
        """|1:19:error: expected Type, got 1 of type Int
           |{ def f (x : Int) 1 = { x = 1 } f(0) }
           |                  ^
           |1:23:error: expected 1, got { x = 1 } of type { x : Int }
           |{ def f (x : Int) 1 = { x = 1 } f(0) }
           |                      ^
           |"""
    )

    test(
        "bad function definition return type",
        "{ def f (x : Int) Int = { x = 1 } f(0) }",
        """|1:25:error: expected Int, got { x = 1 } of type { x : Int }
           |{ def f (x : Int) Int = { x = 1 } f(0) }
           |                        ^
           |"""
    )

    test(
        "record field type that isn't a type",
        "{fun (r : { x : 1 }) 0}({ x = 1 })",
        """|1:17:error: expected Type, got 1 of type Int
           |{fun (r : { x : 1 }) 0}({ x = 1 })
           |                ^
           |1:25:error: expected { x : 1 }, got { x = 1 } of type { x : Int }
           |{fun (r : { x : 1 }) 0}({ x = 1 })
           |                        ^
           |"""
    )

    test(
        "variant field type that isn't a type",
        "{fun (r : << x : 1 >>) 0}(<< x = 1 >>)",
        """|1:18:error: expected Type, got 1 of type Int
           |{fun (r : << x : 1 >>) 0}(<< x = 1 >>)
           |                 ^
           |1:27:error: expected << x : 1 >>, got << x = 1 >> of type << x : Int >>
           |{fun (r : << x : 1 >>) 0}(<< x = 1 >>)
           |                          ^
           |"""
    )

    test(
        "subtype record function argument",
        "{fun (r : { x : Int }) 0}({ x = 1, y = 2 })",
        ""
    )

    test(
        "subtype record function definition argument",
        "{ def f (r : { x : Int }) Int = 0 f({ x = 1, y = 2 }) }",
        ""
    )

    test(
        "bad subtype record function argument",
        "{fun (r : { x : Int, y : Int }) 0}({ x = 1 })",
        """|1:36:error: expected { x : Int, y : Int }, got { x = 1 } of type { x : Int }
           |{fun (r : { x : Int, y : Int }) 0}({ x = 1 })
           |                                   ^
           |"""
    )

    test(
        "bad subtype record function definition argument",
        "{ def f (r : { x : Int, y : Int }) Int = 0 f({ x = 1 }) }",
        """|1:46:error: expected { x : Int, y : Int }, got { x = 1 } of type { x : Int }
           |{ def f (r : { x : Int, y : Int }) Int = 0 f({ x = 1 }) }
           |                                             ^
           |"""
    )

    test(
        "subtype function function argument",
        "{fun (r : ({ x : Int, y : Int }) Int) 0}(fun (s : { x : Int }) s.x)",
        ""
    )

    test(
        "subtype function function definition argument",
        "{ def f (r : ({ x : Int, y : Int }) Int) Int = 0 f(fun (s : { x : Int }) 0) }",
        ""
    )

    test(
        "bad subtype function function argument",
        "{fun (r : ({ x : Int }) Int) 0}(fun (s : { x : Int, y : Int }) s.x)",
        """|1:33:error: expected ({ x : Int }) Int, got fun (s : { x : Int, y : Int }) s.x of type (s : { x : Int, y : Int }) Int
           |{fun (r : ({ x : Int }) Int) 0}(fun (s : { x : Int, y : Int }) s.x)
           |                                ^
           |"""
    )

    test(
        "bad subtype function function definition argument",
        "{ def f (r : ({ x : Int }) Int) Int = 0 f(fun (s : { x : Int, y : Int }) s.x) }",
        """|1:43:error: expected ({ x : Int }) Int, got fun (s : { x : Int, y : Int }) s.x of type (s : { x : Int, y : Int }) Int
           |{ def f (r : ({ x : Int }) Int) Int = 0 f(fun (s : { x : Int, y : Int }) s.x) }
           |                                          ^
           |"""
    )

}
