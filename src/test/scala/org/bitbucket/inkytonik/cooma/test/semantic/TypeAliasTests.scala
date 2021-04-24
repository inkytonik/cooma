package org.bitbucket.inkytonik.cooma.test.semantic

import org.bitbucket.inkytonik.cooma.test.SemanticTests

class TypeAliasTests extends SemanticTests {

    test(
        "non-type name used as argument type",
        "{ val x = 1 fun (y : x) y }",
        """|1:22:error: expected Type, got x of type Int
           |{ val x = 1 fun (y : x) y }
           |                     ^
           |"""
    )

    test(
        "non-type name used as argument type in function type",
        "{ val x = 1 fun (y : (x) Int) y }",
        """|1:23:error: expected Type, got x of type Int
           |{ val x = 1 fun (y : (x) Int) y }
           |                      ^
           |"""
    )

    test(
        "non-type name used as return type in function type",
        "{ val x = 1 fun (y : (Int) x) y }",
        """|1:28:error: expected Type, got x of type Int
           |{ val x = 1 fun (y : (Int) x) y }
           |                           ^
           |"""
    )

    test(
        "non-type name used as field type",
        "{ val x = 1 fun (y : {a : x}) 1 }",
        """|1:27:error: expected Type, got x of type Int
           |{ val x = 1 fun (y : {a : x}) 1 }
           |                          ^
           |"""
    )

    test(
        "alias of simple type as argument",
        "{ type Foo = Int {fun (x : Foo) 0}(1) }",
        ""
    )

    test(
        "alias of simple type as return type",
        "{ type Foo = Int def f (x : Int) Foo = 0 f(1) }",
        ""
    )

    test(
        "alias of record type",
        "{ type Foo = { x : Int, y : String } fun (f : Foo) f.x }",
        ""
    )

    test(
        "bad concat of aliased record types",
        """{
          |   type Foo = { x : Int, y : String }
          |   type Bar = { x : Int }
          |   fun (f : Foo, b : Bar) f & b
          |}""",
        """|4:27:error: record concatenation has overlapping field(s) x
           |   fun (f : Foo, b : Bar) f & b
           |                          ^
           |"""
    )

    test(
        "alias of function type",
        "{ type Foo = (Int) String fun (f : Foo) f(0) }",
        ""
    )

    test(
        "alias of alias of simple type",
        "{ type Foo = Int val Bar = Foo {fun (x : Bar) 0}(1) }",
        ""
    )

    test(
        "alias of record type of alias",
        """{
                      type Foo = Int
                      type Bar = { f : Foo }
                      def m (x : Bar) Int = x.f
                      0
                   }""",
        ""
    )

    test(
        "argument alias of record type with nested alias",
        """{
                      type Foo = Int
                      type Bar = { f : (Foo) Int }
                      def m (x : Bar) Int = x.f(1)
                      0
                   }""",
        ""
    )

    test(
        "return alias of variant type with nested alias",
        """{
                      type Foo = Int
                      type Ble = { a : Int }
                      type Bar = <f : (Foo) Ble>
                      def m (x : Int) Bar =
                         <f = fun (y : Foo) {a = 3}>
                      0
                   }""",
        ""
    )

    test(
        "ok match alias of variant type",
        """{
                      type Foo = <f : Unit>
                      def m (x : Foo) Int =
                         x match { case f(a) => 10 }
                      0
                   }""",
        ""
    )

    test(
        "bad match alias of variant type",
        """{
          |   type Foo = <f : Unit>
          |   def m (x : Foo) Int =
          |     x match { case g(a) => 10 }
          |   0
          |}""",
        """|4:16:error: variant g not present in matched type < f : Unit >
           |     x match { case g(a) => 10 }
           |               ^
           |"""
    )

    test(
        "ok aliased case branches",
        """{
                      type Foo = Int
                      type Bar = Int
                      def m (v : <a : Int, b : Int>, x : Foo, y : Bar) Int =
                        v match { case a(c) => x case b(d) => y }
                      0
                   }""",
        ""
    )

    test(
        "bad aliased case branches",
        """{
          |   type Foo = Int
          |   type Bar = String
          |   def m (v : <a : Int, b : Int>, x : Foo, y : Bar) Int =
          |      v match { case a(c) => x case b(d) => y }
          |   0
          |}""",
        """|5:30:error: case expression types are not the same
           |      v match { case a(c) => x case b(d) => y }
           |                             ^
           |5:45:error: case expression types are not the same
           |      v match { case a(c) => x case b(d) => y }
           |                                            ^
           |"""
    )

    test(
        "alias of function type of alias",
        """{
                      type Foo = Int
                      type Bar = (Foo) Foo
                      def m (x : Bar) Int = x(0)
                      0
                   }""",
        ""
    )

    test(
        "alias of not-declared type",
        "{ type Foo = Bar 0 }",
        """|1:14:error: Bar is not declared
           |{ type Foo = Bar 0 }
           |             ^
           |"""
    )

    test(
        "alias of self",
        "{ type Foo = Foo 0 }",
        """|1:14:error: Foo is not declared
           |{ type Foo = Foo 0 }
           |             ^
           |"""
    )

}
