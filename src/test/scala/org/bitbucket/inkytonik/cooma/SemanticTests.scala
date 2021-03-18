/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.cooma

import org.bitbucket.inkytonik.kiama.util.Tests

class SemanticTests extends Tests {

    import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
    import org.bitbucket.inkytonik.kiama.relation.Tree
    import org.bitbucket.inkytonik.kiama.util.StringSource

    val driver = new ReferenceDriver
    val config = {
        val newConfig = driver.createConfig(Seq())
        newConfig.verify()
        newConfig
    }

    case class SemanticTest(
        name : String,
        expression : String,
        expectedMessages : String
    )

    val semanticTests =
        Vector(
            // Definitions

            SemanticTest(
                "illegal main program argument type (single)",
                "fun (x : Int) x",
                """|1:10:error: illegal main program argument type
                   |fun (x : Int) x
                   |         ^
                   |"""
            ),
            SemanticTest(
                "illegal main program argument type (multiple)",
                "fun (x : Boolean, y : Int) x",
                """|1:10:error: illegal main program argument type
                   |fun (x : Boolean, y : Int) x
                   |         ^
                   |1:23:error: illegal main program argument type
                   |fun (x : Boolean, y : Int) x
                   |                      ^
                   |"""
            ),
            SemanticTest(
                "ok main program argument type (indirect)",
                "{fun (x : Int) fun (_ : Int) x}(0)",
                ""
            ),
            SemanticTest(
                "distinct argument names (fun)",
                "{fun (x : Int, y : Int) x}(0, 0)",
                ""
            ),
            SemanticTest(
                "duplicated underscore argument names (fun)",
                "{fun (_ : Int, _ : Int) 3}(0, 0)",
                ""
            ),
            SemanticTest(
                "duplicated argument name (fun)",
                "{fun (x : Int, x : Int) x}(0, 0)",
                """|1:16:error: re-declaration of x
                   |{fun (x : Int, x : Int) x}(0, 0)
                   |               ^
                   |"""
            ),
            SemanticTest(
                "distinct fields",
                "{ x = 1, y = 1 }",
                ""
            ),
            SemanticTest(
                "duplicate normal fields",
                "{ x = 1, x = 1 }",
                """|1:3:error: duplicate field x
                   |{ x = 1, x = 1 }
                   |  ^
                   |1:10:error: duplicate field x
                   |{ x = 1, x = 1 }
                   |         ^
                   |"""
            ),
            SemanticTest(
                "duplicated variant name in variant type",
                "{fun (a : < x : Int, x : Int, y : Int >) a}(<x = 3>)",
                """|1:13:error: duplicate type field x
                   |{fun (a : < x : Int, x : Int, y : Int >) a}(<x = 3>)
                   |            ^
                   |1:22:error: duplicate type field x
                   |{fun (a : < x : Int, x : Int, y : Int >) a}(<x = 3>)
                   |                     ^
                   |"""
            ),
            SemanticTest(
                "distinct type fields",
                "{fun (a : { x : Int, y : Int }) 0}({x = 1, y = 2})",
                ""
            ),
            SemanticTest(
                "duplicate type fields",
                "{fun (a : { x : Int, x : Int }) 0}({x = 1, x = 2})",
                """|1:13:error: duplicate type field x
                   |{fun (a : { x : Int, x : Int }) 0}({x = 1, x = 2})
                   |            ^
                   |1:22:error: duplicate type field x
                   |{fun (a : { x : Int, x : Int }) 0}({x = 1, x = 2})
                   |                     ^
                   |1:37:error: duplicate field x
                   |{fun (a : { x : Int, x : Int }) 0}({x = 1, x = 2})
                   |                                    ^
                   |1:44:error: duplicate field x
                   |{fun (a : { x : Int, x : Int }) 0}({x = 1, x = 2})
                   |                                           ^
                   |"""
            ),
            SemanticTest(
                "underscore val",
                "{ val _ = 3 0 }",
                ""
            ),
            SemanticTest(
                "distinct function names",
                "{ def f (i : Int) Int = i def g (i : Int) Int = i 0 }",
                ""
            ),
            SemanticTest(
                "duplicate function names in same group",
                "{ def f (i : Int) Int = i def f (i : Int) Int = i 0 }",
                """|1:31:error: re-declaration of f
                   |{ def f (i : Int) Int = i def f (i : Int) Int = i 0 }
                   |                              ^
                   |"""
            ),
            SemanticTest(
                "distinct argument names (def)",
                "{ def f (x : Int, y : Int) Int = x 0 }",
                ""
            ),
            SemanticTest(
                "duplicated underscore argument names (def)",
                "{ def f (_ : Int, _ : Int) Int = 0 0 }",
                ""
            ),
            SemanticTest(
                "duplicated argument names (def)",
                "{ def f (x : Int, x : Int) Int = x 0 }",
                """|1:19:error: re-declaration of x
                   |{ def f (x : Int, x : Int) Int = x 0 }
                   |                  ^
                   |"""
            ),

            // Uses

            SemanticTest(
                "val not usable in its own type",
                "{ val x : x = 0 1 }",
                """|1:11:error: x is not declared
                   |{ val x : x = 0 1 }
                   |          ^
                   |"""
            ),
            SemanticTest(
                "val not usable in its own definition",
                "{ val x = x 1 }",
                """|1:11:error: x is not declared
                   |{ val x = x 1 }
                   |          ^
                   |"""
            ),
            SemanticTest(
                "underscore val not usable",
                "{ val _ = 3 _ }",
                """|1:13:error: _ is not declared
                   |{ val _ = 3 _ }
                   |            ^
                   |"""
            ),
            SemanticTest(
                "underscore argument not usable (fun)",
                "{fun (_ : Int) _}(0)",
                """|1:16:error: _ is not declared
                   |{fun (_ : Int) _}(0)
                   |               ^
                   |"""
            ),
            SemanticTest(
                "underscore argument not usable (def)",
                "{ def f (_ : Int) Int = _ 0 }",
                """|1:25:error: _ is not declared
                   |{ def f (_ : Int) Int = _ 0 }
                   |                        ^
                   |"""
            ),
            SemanticTest(
                "underscore def not usable",
                "{ def _ (x : Int) Int = 0 _(1) }",
                """|1:27:error: _ is not declared
                   |{ def _ (x : Int) Int = 0 _(1) }
                   |                          ^
                   |"""
            ),
            SemanticTest(
                "lone name",
                "x",
                """|1:1:error: x is not declared
                   |x
                   |^
                   |"""
            ),
            SemanticTest(
                "non-declared name in field definition (record first)",
                "{ x = y }",
                """|1:7:error: y is not declared
                   |{ x = y }
                   |      ^
                   |"""
            ),
            SemanticTest(
                "non-declared name in field definition (record second)",
                "{ x = 1, y = z }",
                """|1:14:error: z is not declared
                   |{ x = 1, y = z }
                   |             ^
                   |"""
            ),
            SemanticTest(
                "non-declared name in field definition (variant)",
                "< x = y >",
                """|1:7:error: y is not declared
                   |< x = y >
                   |      ^
                   |"""
            ),
            SemanticTest(
                "declared value name",
                "{ val x = 1 x}",
                ""
            ),
            SemanticTest(
                "redeclared value name",
                "{ val x = 1 val x = 2 x}",
                ""
            ),
            SemanticTest(
                "not-declared value name",
                "{ val x = 1 y}",
                """|1:13:error: y is not declared
                   |{ val x = 1 y}
                   |            ^
                   |"""
            ),
            SemanticTest(
                "declared argument name",
                "{fun (x : Int) x}(0)",
                ""
            ),
            SemanticTest(
                "argument name not in argument type scope",
                "{fun (x : x) 0}(0)",
                """|1:11:error: x is not declared
                   |{fun (x : x) 0}(0)
                   |          ^
                   |"""
            ),
            SemanticTest(
                "not-declared use in no argument function",
                "{fun () x}()",
                """|1:9:error: x is not declared
                   |{fun () x}()
                   |        ^
                   |"""
            ),
            SemanticTest(
                "not-declared use in argument function",
                "{fun (x : Int) y}(0)",
                """|1:16:error: y is not declared
                   |{fun (x : Int) y}(0)
                   |               ^
                   |"""
            ),
            SemanticTest(
                "not-declared use in nested expression",
                "{fun (x : { a : Int }) x & y}({a = 1})",
                """|1:28:error: y is not declared
                   |{fun (x : { a : Int }) x & y}({a = 1})
                   |                           ^
                   |"""
            ),
            SemanticTest(
                "not-declared use in field definition",
                "fun () { a = y }",
                """|1:14:error: y is not declared
                   |fun () { a = y }
                   |             ^
                   |"""
            ),
            SemanticTest(
                "not-declared use in value definition (self)",
                "{ val x = x 0 }",
                """|1:11:error: x is not declared
                   |{ val x = x 0 }
                   |          ^
                   |"""
            ),
            SemanticTest(
                "not-declared use in value definition (other)",
                "{ val x = y 0 }",
                """|1:11:error: y is not declared
                   |{ val x = y 0 }
                   |          ^
                   |"""
            ),
            SemanticTest(
                "overriding value names",
                """{ val x = "hi" val x = 2 {fun (a : Int) 0}(x) }""",
                ""
            ),
            SemanticTest(
                "not-declared use in no argument function definition",
                "{ def f () Int = x 0 }",
                """|1:18:error: x is not declared
                   |{ def f () Int = x 0 }
                   |                 ^
                   |"""
            ),
            SemanticTest(
                "not-declared use in argument function definition",
                "{ def f (x : Int) Int = y 0 }",
                """|1:25:error: y is not declared
                   |{ def f (x : Int) Int = y 0 }
                   |                        ^
                   |"""
            ),
            SemanticTest(
                "not-declared use as type name in function",
                "{fun (x : Foo) 0}(0)",
                """|1:11:error: Foo is not declared
                   |{fun (x : Foo) 0}(0)
                   |          ^
                   |"""
            ),
            SemanticTest(
                "not-declared use as type name in function defintion",
                "{ def f (x : Foo) Int = 0 0 }",
                """|1:14:error: Foo is not declared
                   |{ def f (x : Foo) Int = 0 0 }
                   |             ^
                   |"""
            ),
            SemanticTest(
                "overriding function names in different group",
                "{ def f (i : String) Int = 0 val x = 1 def f (i : Int) Int = i f(0) }",
                ""
            ),
            SemanticTest(
                "ok self reference function in same group",
                "{ def f(x : Int) Int = f(x) f(10) }",
                ""
            ),
            SemanticTest(
                "def use (far, one group)",
                "{ def f(x : Int) Int = x def g(x : Int) Int = x f(10) }",
                ""
            ),
            SemanticTest(
                "def use (near, one group)",
                "{ def f(x : Int) Int = x def g(x : Int) Int = x g(10) }",
                ""
            ),
            SemanticTest(
                "def use (far, two groups)",
                "{ def f(x : Int) Int = x val x = 1 def g(x : Int) Int = x f(10) }",
                ""
            ),
            SemanticTest(
                "ok forward reference function in same group",
                "{ def f(x : Int) Int = g(x) def g(x : Int) Int = x f(10) }",
                ""
            ),
            SemanticTest(
                "bad forward reference function to different group",
                "{ def f(x : Int) Int = g(x) val x = 1 def g(x : Int) Int = x f(10) }",
                """|1:24:error: g is not declared
                   |{ def f(x : Int) Int = g(x) val x = 1 def g(x : Int) Int = x f(10) }
                   |                       ^
                   |"""
            ),
            SemanticTest(
                "ok backward reference function in same group",
                "{ def f(x : Int) Int = x def g(x : Int) Int = f(x) g(10) }",
                ""
            ),
            SemanticTest(
                "ok backward reference function to different group",
                "{ def f(x : Int) Int = x val x = 1 def g(x : Int) Int = f(x) g(10) }",
                ""
            ),

            // Application

            SemanticTest(
                "apply no-arg function",
                "{fun () 0}()",
                ""
            ),
            SemanticTest(
                "apply function to too many arguments (zero)",
                "{fun () 0}(3)",
                """|1:12:error: expected no arguments, got 1
                   |{fun () 0}(3)
                   |           ^
                   |"""
            ),
            SemanticTest(
                "apply function to too many arguments (one)",
                "{fun (x : Int) 0}(2, 3)",
                """|1:19:error: expected up to one argument, got 2
                   |{fun (x : Int) 0}(2, 3)
                   |                  ^
                   |"""
            ),
            SemanticTest(
                "apply function to too many arguments (more than one)",
                "{fun (x : Int, y : Int) 0}(2, 3, 4)",
                """|1:28:error: expected up to 2 arguments, got 3
                   |{fun (x : Int, y : Int) 0}(2, 3, 4)
                   |                           ^
                   |"""
            ),
            SemanticTest(
                "application of non-function",
                "4(2, 3)",
                """|1:1:error: application of non-function type Int
                   |4(2, 3)
                   |^
                   |"""
            ),
            SemanticTest(
                "type argument",
                "{fun (t : Type, x : t) x}",
                ""
            ),
            SemanticTest(
                "bad type application",
                "{fun (x : Int, t : Type) x}(Int, 10)",
                """|1:29:error: expected Int, got Int of type Type
                   |{fun (x : Int, t : Type) x}(Int, 10)
                   |                            ^
                   |1:34:error: expected Type, got 10 of type Int
                   |{fun (x : Int, t : Type) x}(Int, 10)
                   |                                 ^
                   |"""
            ),

            // Built-in uses

            SemanticTest(
                "Boolean is defined",
                "Boolean",
                ""
            ),
            SemanticTest(
                "Boolean is a variant with False and True fields",
                "{fun (b : Boolean) b match { case False(x) => 0 case True(x) => 1}}(true)",
                ""
            ),
            SemanticTest(
                "false is defined",
                "false",
                ""
            ),
            SemanticTest(
                "false is Boolean",
                "{fun (b : Boolean) 0}(false)",
                ""
            ),
            SemanticTest(
                "true is defined",
                "true",
                ""
            ),
            SemanticTest(
                "true is Boolean",
                "{fun (b : Boolean) 0}(true)",
                ""
            ),
            SemanticTest(
                "aliases are used in type error messages",
                "{fun (b : Boolean) 0}(0)",
                """|1:23:error: expected Boolean, got 0 of type Int
                   |{fun (b : Boolean) 0}(0)
                   |                      ^
                   |"""
            ),
            SemanticTest(
                "HttpGet is a record with a get field",
                "fun (httpClient : HttpGet) httpClient.get(\"\")",
                ""
            ),
            SemanticTest(
                "HttpDelete & HttpGet & HttpPost & HttpPut is a record with the respective fields",
                """|fun (httpClient : HttpDelete & HttpGet & HttpPost & HttpPut) {
                   |    val _ = httpClient.delete("")
                   |    val _ = httpClient.get("")
                   |    val _ = httpClient.post("")
                   |    val _ = httpClient.put("")
                   |    {}
                   |}
                   |""".stripMargin,
                ""
            ),
            SemanticTest(
                "Reader is a record with a read field",
                "fun (r : Reader) r.read()",
                ""
            ),
            SemanticTest(
                "Reader read field has correct type",
                "{ def f (r : Reader) String = r.read() 0 }",
                ""
            ),
            SemanticTest(
                "Reader doesn't have non-read field",
                "fun (r : Reader) r.foo",
                """|1:20:error: foo is not a field of record type Reader
                   |fun (r : Reader) r.foo
                   |                   ^
                   |"""
            ),
            SemanticTest(
                "Writer is built-in record type",
                """fun (w : Writer) w.write("hello")""",
                ""
            ),
            SemanticTest(
                "Writer write field has correct type",
                """{ def f (w : Writer) Unit = w.write("hi") 0 }""",
                ""
            ),
            SemanticTest(
                "Writer doesn't have non-write field",
                "fun (w : Writer) w.foo",
                """|1:20:error: foo is not a field of record type Writer
                   |fun (w : Writer) w.foo
                   |                   ^
                   |"""
            ),

            // Selection

            SemanticTest(
                "existent field (one)",
                "{ x = 3 }.x",
                ""
            ),
            SemanticTest(
                "existent field (many)",
                "{ x = 3, y = 4, z = 5 }.y",
                ""
            ),
            SemanticTest(
                "non-existent field (one)",
                "{ x = 3 }.y",
                """|1:11:error: y is not a field of record type { x : Int }
                   |{ x = 3 }.y
                   |          ^
                   |"""
            ),
            SemanticTest(
                "non-existent field (many)",
                "{ x = 3, y = 4, z = 5 }.w",
                """|1:25:error: w is not a field of record type { x : Int, y : Int, z : Int }
                   |{ x = 3, y = 4, z = 5 }.w
                   |                        ^
                   |"""
            ),
            SemanticTest(
                "selection from non-record",
                "42.x",
                """|1:4:error: selection of x field from non-record type Int
                   |42.x
                   |   ^
                   |"""
            ),

            // Matching

            SemanticTest(
                "basic match (bind)",
                "< x = 1 > match { case x(a) => a }",
                ""
            ),
            SemanticTest(
                "basic match (wildcard)",
                "< x = 1 > match { case x(_) => 1 }",
                ""
            ),
            SemanticTest(
                "basic match (wildcard not usable)",
                "< x = 1 > match { case x(_) => _ }",
                """|1:32:error: _ is not declared
                   |< x = 1 > match { case x(_) => _ }
                   |                               ^
                   |"""
            ),
            SemanticTest(
                "basic match correct type",
                """{
                     def f () Int = < x = 1 > match { case x(a) => a }
                     f ()
                   }""",
                ""
            ),
            SemanticTest(
                "match of non-variant",
                "3 match { case x(a) => a }",
                """|1:1:error: match of non-variant type Int
                   |3 match { case x(a) => a }
                   |^
                   |"""
            ),
            SemanticTest(
                "basic match wrong result type",
                """{
                  |  def f () String = < x = 1 > match { case x(a) => a }
                  |  f ()
                  |}""",
                """|2:21:error: expected String, got < x = 1 > match { case x(a) => a } of type Int
                   |  def f () String = < x = 1 > match { case x(a) => a }
                   |                    ^
                   |"""
            ),
            SemanticTest(
                "non-declared name in match case",
                "< x = 1 > match { case x(a) => y }",
                """|1:32:error: y is not declared
                   |< x = 1 > match { case x(a) => y }
                   |                               ^
                   |"""
            ),
            SemanticTest(
                "correct number and type of cases for match",
                """{
                    def f () <x : Int, y : Int> = <x = 3>
                    f () match { case x(a) => 1 case y(b) => 2 }
                }""",
                ""
            ),
            SemanticTest(
                "correct number of cases but wrong type for match",
                """{
                  |  def f () <x : Int, y : Int> = <x = 3>
                  |  f () match { case x(a) => 1 case y(b) => "hi" }
                  |}""",
                """|3:29:error: case expression types are not the same
                   |  f () match { case x(a) => 1 case y(b) => "hi" }
                   |                            ^
                   |3:44:error: case expression types are not the same
                   |  f () match { case x(a) => 1 case y(b) => "hi" }
                   |                                           ^
                   |"""
            ),
            SemanticTest(
                "incorrect number of cases for match",
                """{
                  |  def f () < x : Int, y : Int > = < x = 3 >
                  |  f () match { case x(a) => 1 }
                  |}""",
                """|3:16:error: expected 2 cases, got 1
                   |  f () match { case x(a) => 1 }
                   |               ^
                   |"""
            ),
            SemanticTest(
                "duplicate cases for match",
                """{
                  |  def f () < x : Int, y : Int > = < x = 3 >
                  |  f () match { case x(a) => 1 case x(b) => 2 }
                  |}""",
                """|3:16:error: duplicate case for variant x
                   |  f () match { case x(a) => 1 case x(b) => 2 }
                   |               ^
                   |3:31:error: duplicate case for variant x
                   |  f () match { case x(a) => 1 case x(b) => 2 }
                   |                              ^
                   |"""
            ),
            SemanticTest(
                "incorrect variant for match",
                """{
                  |  def f () < x : Int, y : Int > = < x = 3 >
                  |  f () match { case w(a) => 1 case y(b) => 2 }
                  |}""",
                """|3:16:error: variant w not present in matched type < x : Int, y : Int >
                   |  f () match { case w(a) => 1 case y(b) => 2 }
                   |               ^
                   |"""
            ),

            // Type names

            SemanticTest(
                "non-type name used as argument type",
                "{ val x = 1 fun (y : x) y }",
                """|1:22:error: expected Type, got x of type Int
                   |{ val x = 1 fun (y : x) y }
                   |                     ^
                   |"""
            ),
            SemanticTest(
                "non-type name used as argument type in function type",
                "{ val x = 1 fun (y : (x) Int) y }",
                """|1:23:error: expected Type, got x of type Int
                   |{ val x = 1 fun (y : (x) Int) y }
                   |                      ^
                   |"""
            ),
            SemanticTest(
                "non-type name used as return type in function type",
                "{ val x = 1 fun (y : (Int) x) y }",
                """|1:28:error: expected Type, got x of type Int
                   |{ val x = 1 fun (y : (Int) x) y }
                   |                           ^
                   |"""
            ),
            SemanticTest(
                "non-type name used as field type",
                "{ val x = 1 fun (y : {a : x}) 1 }",
                """|1:27:error: expected Type, got x of type Int
                   |{ val x = 1 fun (y : {a : x}) 1 }
                   |                          ^
                   |"""
            ),

            // type aliases

            SemanticTest(
                "alias of simple type as argument",
                "{ type Foo = Int {fun (x : Foo) 0}(1) }",
                ""
            ),
            SemanticTest(
                "alias of simple type as return type",
                "{ type Foo = Int def f (x : Int) Foo = 0 f(1) }",
                ""
            ),
            SemanticTest(
                "alias of record type",
                "{ type Foo = { x : Int, y : String } fun (f : Foo) f.x }",
                ""
            ),
            SemanticTest(
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
            ),
            SemanticTest(
                "alias of function type",
                "{ type Foo = (Int) String fun (f : Foo) f(0) }",
                ""
            ),
            SemanticTest(
                "alias of alias of simple type",
                "{ type Foo = Int val Bar = Foo {fun (x : Bar) 0}(1) }",
                ""
            ),
            SemanticTest(
                "alias of record type of alias",
                """{
                      type Foo = Int
                      type Bar = { f : Foo }
                      def m (x : Bar) Int = x.f
                      0
                   }""",
                ""
            ),
            SemanticTest(
                "argument alias of record type with nested alias",
                """{
                      type Foo = Int
                      type Bar = { f : (Foo) Int }
                      def m (x : Bar) Int = x.f(1)
                      0
                   }""",
                ""
            ),
            SemanticTest(
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
            ),
            SemanticTest(
                "ok match alias of variant type",
                """{
                      type Foo = <f : Unit>
                      def m (x : Foo) Int =
                         x match { case f(a) => 10 }
                      0
                   }""",
                ""
            ),
            SemanticTest(
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
            ),
            SemanticTest(
                "ok aliased case branches",
                """{
                      type Foo = Int
                      type Bar = Int
                      def m (v : <a : Int, b : Int>, x : Foo, y : Bar) Int =
                        v match { case a(c) => x case b(d) => y }
                      0
                   }""",
                ""
            ),
            SemanticTest(
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
            ),
            SemanticTest(
                "alias of function type of alias",
                """{
                      type Foo = Int
                      type Bar = (Foo) Foo
                      def m (x : Bar) Int = x(0)
                      0
                   }""",
                ""
            ),
            SemanticTest(
                "alias of not-declared type",
                "{ type Foo = Bar 0 }",
                """|1:14:error: Bar is not declared
                   |{ type Foo = Bar 0 }
                   |             ^
                   |"""
            ),
            SemanticTest(
                "alias of self",
                "{ type Foo = Foo 0 }",
                """|1:14:error: Foo is not declared
                   |{ type Foo = Foo 0 }
                   |             ^
                   |"""
            ),

            // Expected types

            // - val definitions

            SemanticTest(
                "val explicit type must be a type",
                "{ val x : 7 = 1 x }",
                """|1:11:error: expected Type, got 7 of type Int
                   |{ val x : 7 = 1 x }
                   |          ^
                   |1:15:error: expected 7, got 1 of type Int
                   |{ val x : 7 = 1 x }
                   |              ^
                   |"""
            ),
            SemanticTest(
                "val explicit simple type (ok)",
                "{ val x : Int = 1 x }",
                ""
            ),
            SemanticTest(
                "val explicit simple type (bad)",
                "{ val x : String = 1 x }",
                """|1:20:error: expected String, got 1 of type Int
                   |{ val x : String = 1 x }
                   |                   ^
                   |"""
            ),
            SemanticTest(
                "val explicit function type (ok)",
                "{ val f : (Int) Int = fun (x : Int) x f(0)}",
                ""
            ),
            SemanticTest(
                "val explicit function type (bad)",
                """{ val f : (String) Int = fun (x : Int) x f("hi")}""",
                """|1:26:error: expected (String) Int, got fun (x : Int) x of type (x : Int) Int
                   |{ val f : (String) Int = fun (x : Int) x f("hi")}
                   |                         ^
                   |"""
            ),
            SemanticTest(
                "val explicit record type (ok)",
                "{ val x : { a : Int, b : Int } = { a = 1, b = 2 } x }",
                ""
            ),
            SemanticTest(
                "val explicit record type (bad)",
                "{ val x : { a : Int, b : String } = { a = 1 } x }",
                """|1:37:error: expected { a : Int, b : String }, got { a = 1 } of type { a : Int }
                   |{ val x : { a : Int, b : String } = { a = 1 } x }
                   |                                    ^
                   |"""
            ),
            SemanticTest(
                "val explicit variant type (ok)",
                "{ val x : < a : Int, b : String > = < a = 1 > x }",
                ""
            ),
            SemanticTest(
                "val explicit variant type (bad)",
                "{ val x : < a : Int, b : String > = < c = 1 > x }",
                """|1:37:error: expected < a : Int, b : String >, got < c = 1 > of type < c : Int >
                   |{ val x : < a : Int, b : String > = < c = 1 > x }
                   |                                    ^
                   |"""
            ),
            SemanticTest(
                "val explicit pre=defined type (ok)",
                "{ val x : Boolean = < True = {} > x }",
                ""
            ),
            SemanticTest(
                "val explicit pre=defined type (bad)",
                "{ val x : Boolean = 1 x }",
                """|1:21:error: expected Boolean, got 1 of type Int
                   |{ val x : Boolean = 1 x }
                   |                    ^
                   |"""
            ),

            // - ok arguments

            SemanticTest(
                "function argument type same (one)",
                "{fun (x : Int) x}(1)",
                ""
            ),
            SemanticTest(
                "function argument type same (two)",
                "{fun (x : Int, y : Int) x}(1, 2)",
                ""
            ),
            SemanticTest(
                "function definition argument type same (one)",
                "{ def f (x : Int) Int = x f(1) }",
                ""
            ),
            SemanticTest(
                "function definition argument type same (two)",
                "{ def f (x : Int, y : Int) Int = x f(1, 2) }",
                ""
            ),

            // - bad arguments

            SemanticTest(
                "function argument type that isn't a type",
                "{fun (x : 3) x}(1)",
                """|1:11:error: expected Type, got 3 of type Int
                   |{fun (x : 3) x}(1)
                   |          ^
                   |1:17:error: expected 3, got 1 of type Int
                   |{fun (x : 3) x}(1)
                   |                ^
                   |"""
            ),
            SemanticTest(
                "bad function argument type (one, simple)",
                "{fun (x : String) x}(1)",
                """|1:22:error: expected String, got 1 of type Int
                   |{fun (x : String) x}(1)
                   |                     ^
                   |"""
            ),
            SemanticTest(
                "bad function argument type (two, simple)",
                "{fun (x : Int, y : String) x}(1, 2)",
                """|1:34:error: expected String, got 2 of type Int
                   |{fun (x : Int, y : String) x}(1, 2)
                   |                                 ^
                   |"""
            ),
            SemanticTest(
                "bad function argument type (record formal)",
                "{fun (x : { y : Int }) x}(1)",
                """|1:27:error: expected { y : Int }, got 1 of type Int
                   |{fun (x : { y : Int }) x}(1)
                   |                          ^
                   |"""
            ),
            SemanticTest(
                "bad function argument type (record actual)",
                "{fun (x : Int) x}({ y = 1 })",
                """|1:19:error: expected Int, got { y = 1 } of type { y : Int }
                   |{fun (x : Int) x}({ y = 1 })
                   |                  ^
                   |"""
            ),
            SemanticTest(
                "bad function argument type (function formal)",
                "{fun (x : (Int) String) x}(1)",
                """|1:28:error: expected (Int) String, got 1 of type Int
                   |{fun (x : (Int) String) x}(1)
                   |                           ^
                   |"""
            ),
            SemanticTest(
                "bad function argument type (function actual)",
                "{fun (x : Int) x}(fun (y : Int) y)",
                """|1:19:error: expected Int, got fun (y : Int) y of type (y : Int) Int
                   |{fun (x : Int) x}(fun (y : Int) y)
                   |                  ^
                   |"""
            ),
            SemanticTest(
                "bad function definition argument type (one, simple)",
                "{ def f (x : String) Int = 0 f(1) }",
                """|1:32:error: expected String, got 1 of type Int
                   |{ def f (x : String) Int = 0 f(1) }
                   |                               ^
                   |"""
            ),
            SemanticTest(
                "bad function definition argument type (two, simple)",
                "{ def f (x : Int, y : String) Int = x f(1, 2) }",
                """|1:44:error: expected String, got 2 of type Int
                   |{ def f (x : Int, y : String) Int = x f(1, 2) }
                   |                                           ^
                   |"""
            ),
            SemanticTest(
                "bad function definition argument type (record formal)",
                "{ def f (x : { y : Int }) Int = 0 f(1) }",
                """|1:37:error: expected { y : Int }, got 1 of type Int
                   |{ def f (x : { y : Int }) Int = 0 f(1) }
                   |                                    ^
                   |"""
            ),
            SemanticTest(
                "bad function definition argument type (record actual)",
                "{ def f (x : Int) Int = x f({ y = 1 }) }",
                """|1:29:error: expected Int, got { y = 1 } of type { y : Int }
                   |{ def f (x : Int) Int = x f({ y = 1 }) }
                   |                            ^
                   |"""
            ),
            SemanticTest(
                "bad function definition argument type (function formal)",
                "{ def f (x : (Int) String) Int = 0 f(1) }",
                """|1:38:error: expected (Int) String, got 1 of type Int
                   |{ def f (x : (Int) String) Int = 0 f(1) }
                   |                                     ^
                   |"""
            ),
            SemanticTest(
                "bad function definition argument type (function actual)",
                "{ def f (x : Int) Int = x f(fun (y : Int) y) }",
                """|1:29:error: expected Int, got fun (y : Int) y of type (y : Int) Int
                   |{ def f (x : Int) Int = x f(fun (y : Int) y) }
                   |                            ^
                   |"""
            ),

            // - return types

            SemanticTest(
                "function definition return type that isn't a type",
                "{ def f (x : Int) 1 = { x = 1 } f(0) }",
                """|1:19:error: expected Type, got 1 of type Int
                   |{ def f (x : Int) 1 = { x = 1 } f(0) }
                   |                  ^
                   |1:23:error: expected 1, got { x = 1 } of type { x : Int }
                   |{ def f (x : Int) 1 = { x = 1 } f(0) }
                   |                      ^
                   |"""
            ),
            SemanticTest(
                "bad function definition return type",
                "{ def f (x : Int) Int = { x = 1 } f(0) }",
                """|1:25:error: expected Int, got { x = 1 } of type { x : Int }
                   |{ def f (x : Int) Int = { x = 1 } f(0) }
                   |                        ^
                   |"""
            ),

            // - record and variant field types

            SemanticTest(
                "record field type that isn't a type",
                "{fun (r : { x : 1 }) 0}({ x = 1 })",
                """|1:17:error: expected Type, got 1 of type Int
                   |{fun (r : { x : 1 }) 0}({ x = 1 })
                   |                ^
                   |1:25:error: expected { x : 1 }, got { x = 1 } of type { x : Int }
                   |{fun (r : { x : 1 }) 0}({ x = 1 })
                   |                        ^
                   |"""
            ),
            SemanticTest(
                "variant field type that isn't a type",
                "{fun (r : < x : 1 >) 0}(< x = 1 >)",
                """|1:17:error: expected Type, got 1 of type Int
                   |{fun (r : < x : 1 >) 0}(< x = 1 >)
                   |                ^
                   |1:25:error: expected < x : 1 >, got < x = 1 > of type < x : Int >
                   |{fun (r : < x : 1 >) 0}(< x = 1 >)
                   |                        ^
                   |"""
            ),

            // - subtype arguments

            SemanticTest(
                "subtype record function argument",
                "{fun (r : { x : Int }) 0}({ x = 1, y = 2 })",
                ""
            ),
            SemanticTest(
                "subtype record function definition argument",
                "{ def f (r : { x : Int }) Int = 0 f({ x = 1, y = 2 }) }",
                ""
            ),
            SemanticTest(
                "bad subtype record function argument",
                "{fun (r : { x : Int, y : Int }) 0}({ x = 1 })",
                """|1:36:error: expected { x : Int, y : Int }, got { x = 1 } of type { x : Int }
                   |{fun (r : { x : Int, y : Int }) 0}({ x = 1 })
                   |                                   ^
                   |"""
            ),
            SemanticTest(
                "bad subtype record function definition argument",
                "{ def f (r : { x : Int, y : Int }) Int = 0 f({ x = 1 }) }",
                """|1:46:error: expected { x : Int, y : Int }, got { x = 1 } of type { x : Int }
                   |{ def f (r : { x : Int, y : Int }) Int = 0 f({ x = 1 }) }
                   |                                             ^
                   |"""
            ),
            SemanticTest(
                "subtype function function argument",
                "{fun (r : ({ x : Int, y : Int }) Int) 0}(fun (s : { x : Int }) s.x)",
                ""
            ),
            SemanticTest(
                "subtype function function definition argument",
                "{ def f (r : ({ x : Int, y : Int }) Int) Int = 0 f(fun (s : { x : Int }) 0) }",
                ""
            ),
            SemanticTest(
                "bad subtype function function argument",
                "{fun (r : ({ x : Int }) Int) 0}(fun (s : { x : Int, y : Int }) s.x)",
                """|1:33:error: expected ({ x : Int }) Int, got fun (s : { x : Int, y : Int }) s.x of type (s : { x : Int, y : Int }) Int
                   |{fun (r : ({ x : Int }) Int) 0}(fun (s : { x : Int, y : Int }) s.x)
                   |                                ^
                   |"""
            ),
            SemanticTest(
                "bad subtype function function definition argument",
                "{ def f (r : ({ x : Int }) Int) Int = 0 f(fun (s : { x : Int, y : Int }) s.x) }",
                """|1:43:error: expected ({ x : Int }) Int, got fun (s : { x : Int, y : Int }) s.x of type (s : { x : Int, y : Int }) Int
                   |{ def f (r : ({ x : Int }) Int) Int = 0 f(fun (s : { x : Int, y : Int }) s.x) }
                   |                                          ^
                   |"""
            ),

            // - record operations

            SemanticTest(
                "record concatenation (single)",
                "{ x =  1} & { y = 2 }",
                ""
            ),
            SemanticTest(
                "record concatenation (multiple)",
                "{ w = 0, x =  1} & { a = 2, b = 3, c = 4 }",
                ""
            ),
            SemanticTest(
                "record type concatenation",
                "{ x: Int, y: String } & { z: Int }",
                ""
            ),
            SemanticTest(
                "bad record concatenation (left)",
                "3 & { x = 1 }",
                """|1:1:error: expected record or record type, got Int
                   |3 & { x = 1 }
                   |^
                   |"""
            ),
            SemanticTest(
                "bad record concatenation (right)",
                "{ x = 1 } & 3",
                """|1:13:error: expected record, got Int
                   |{ x = 1 } & 3
                   |            ^
                   |"""
            ),
            SemanticTest(
                "bad record concatenation (both)",
                "3 & 4",
                """|1:1:error: expected record or record type, got Int
                   |3 & 4
                   |^
                   |"""
            ),
            SemanticTest(
                "bad record concatenation (overlapping field)",
                "{ x = 1 } & { y = 1, x = 2 }",
                """|1:1:error: record concatenation has overlapping field(s) x
                   |{ x = 1 } & { y = 1, x = 2 }
                   |^
                   |"""
            ),
            SemanticTest(
                "bad record concatenation (overlapping fields)",
                "{ w = 0, x = 1, y = 2 } & { y = 1, x = 2 }",
                """|1:1:error: record concatenation has overlapping field(s) x, y
                   |{ w = 0, x = 1, y = 2 } & { y = 1, x = 2 }
                   |^
                   |"""
            ),
            SemanticTest(
                "bad record type concatenation (left)",
                "3 & { x: Int }",
                """|1:1:error: expected record or record type, got Int
                   |3 & { x: Int }
                   |^
                   |""".stripMargin
            ),
            SemanticTest(
                "bad record type concatenation (right)",
                "{ x: Int } & 3",
                """|1:14:error: expected record type, got Int
                   |{ x: Int } & 3
                   |             ^
                   |""".stripMargin
            ),
            SemanticTest(
                "bad record type concatenation (overlapping fields)",
                "{ x: Int, y: Int } & { x: Int, y: String, z: Int }",
                """|1:1:error: record concatenation has overlapping field(s) x, y
                   |{ x: Int, y: Int } & { x: Int, y: String, z: Int }
                   |^
                   |""".stripMargin
            ),
            SemanticTest(
                "bad record concatenation (value on left, type on right)",
                "{ x = 1 } & { y: String }",
                """|1:13:error: expected record, got Type
                   |{ x = 1 } & { y: String }
                   |            ^
                   |""".stripMargin
            ),
            SemanticTest(
                "bad record concatenation (type on left, value on right)",
                "{ x: Int } & { y = 4 }",
                """|1:14:error: expected record type, got { y : Int }
                   |{ x: Int } & { y = 4 }
                   |             ^
                   |""".stripMargin
            ),

            // primitives

            SemanticTest(
                s"Partial apply equal (type only)",
                s"equal(Int)",
                ""
            ),
            SemanticTest(
                s"Partial apply equal (type and arg)",
                s"equal(Int, 1)",
                ""
            ),
            SemanticTest(
                s"Wrong number of arguments for Equal primitive (no args)",
                s"prim Equal()",
                s"""|1:1:error: primitive Equal expects 3 arguments got 0
                    |prim Equal()
                    |^
                    |"""
            ),
            SemanticTest(
                s"Wrong number of arguments for Equal primitive (more)",
                s"prim Equal(Int)",
                s"""|1:1:error: primitive Equal expects 3 arguments got 1
                    |prim Equal(Int)
                    |^
                    |"""
            ),
            SemanticTest(
                s"Wrong number of arguments for Equal primitive (more more)",
                s"prim Equal(Int, 2)",
                s"""|1:1:error: primitive Equal expects 3 arguments got 2
                    |prim Equal(Int, 2)
                    |^
                    |"""
            ),
            SemanticTest(
                s"Wrong argument type for Equal primitive (type)",
                s"""prim Equal(1, 1, 1)""",
                s"""|1:12:error: expected Type, got 1 of type Int
                    |prim Equal(1, 1, 1)
                    |           ^
                    |1:15:error: expected 1, got 1 of type Int
                    |prim Equal(1, 1, 1)
                    |              ^
                    |1:18:error: expected 1, got 1 of type Int
                    |prim Equal(1, 1, 1)
                    |                 ^
                    |"""
            ),
            SemanticTest(
                s"Wrong argument type for Equal primitive (value)",
                s"""prim Equal(Int, 1, \"2\")""",
                s"""|1:20:error: expected Int, got "2" of type String
                    |prim Equal(Int, 1, "2")
                    |                   ^
                    |"""
            ),
            SemanticTest(
                "non-existent primitive",
                "prim DoesNotExist(1, 2)",
                """|1:1:error: primitive DoesNotExist not found
                   |prim DoesNotExist(1, 2)
                   |^
                   |"""
            )
        ) ++ Primitives.allInt1PrimBinOps.flatMap(op => {
                val primOp = op.primName
                Vector(
                    SemanticTest(
                        s"Wrong number of arguments for $primOp primitive (no args)",
                        s"prim $primOp()",
                        s"""|1:1:error: primitive $primOp expects 1 arguments got 0
                            |prim $primOp()
                            |^
                            |"""
                    ),
                    SemanticTest(
                        s"Wrong number of arguments for $primOp primitive (more)",
                        s"prim $primOp(2,2)",
                        s"""|1:1:error: primitive $primOp expects 1 arguments got 2
                           |prim $primOp(2,2)
                           |^
                           |"""
                    ),
                    SemanticTest(
                        s"Wrong argument type for $primOp primitive",
                        s"""prim $primOp(\"2\")""",
                        s"""|1:13:error: expected Int, got "2" of type String
                           |prim $primOp("2")
                           |            ^
                           |"""
                    )
                )
            }) ++ Primitives.allInt2PrimBinOps.flatMap(op => {
                val primOp = op.primName
                Vector(
                    SemanticTest(
                        s"Wrong number of arguments for $primOp primitive (no args)",
                        s"prim $primOp()",
                        s"""|1:1:error: primitive $primOp expects 2 arguments got 0
                            |prim $primOp()
                            |^
                            |"""
                    ),
                    SemanticTest(
                        s"Wrong number of arguments for $primOp primitive (less)",
                        s"prim $primOp(2)",
                        s"""|1:1:error: primitive $primOp expects 2 arguments got 1
                            |prim $primOp(2)
                            |^
                            |"""
                    ),
                    SemanticTest(
                        s"Wrong number of arguments for $primOp primitive (more)",
                        s"prim $primOp(2,2,2)",
                        s"""|1:1:error: primitive $primOp expects 2 arguments got 3
                           |prim $primOp(2,2,2)
                           |^
                           |"""
                    ),
                    SemanticTest(
                        s"Wrong argument type for $primOp primitive",
                        s"""prim $primOp(\"2\",2)""",
                        s"""|1:13:error: expected Int, got "2" of type String
                           |prim $primOp("2",2)
                           |            ^
                           |"""
                    ),
                    SemanticTest(
                        s"Wrong argument type (cont) for $primOp primitive",
                        s"""prim $primOp(2,\"2\")""",
                        s"""|1:15:error: expected Int, got "2" of type String
                           |prim $primOp(2,"2")
                           |              ^
                           |"""
                    ),
                    SemanticTest(
                        s"Wrong argument type and number of arguments for $primOp primitive",
                        s"""prim $primOp(2,\"2\",2)""",
                        s"""|1:1:error: primitive $primOp expects 2 arguments got 3
                           |prim $primOp(2,"2",2)
                           |^
                           |1:15:error: expected Int, got "2" of type String
                           |prim $primOp(2,"2",2)
                           |              ^
                           |"""
                    )
                )
            }) ++ Primitives.allIntPrimRelOps.flatMap(op => {
                val primOp = op.primName
                val primCol = 7 + primOp.length
                val primInd = " " * (primCol - 1)
                Vector(
                    SemanticTest(
                        s"Wrong number of arguments for $primOp primitive (no args)",
                        s"prim $primOp()",
                        s"""|1:1:error: primitive $primOp expects 2 arguments got 0
                            |prim $primOp()
                            |^
                            |"""
                    ),
                    SemanticTest(
                        s"Wrong number of arguments for $primOp primitive (less)",
                        s"prim $primOp(2)",
                        s"""|1:1:error: primitive $primOp expects 2 arguments got 1
                            |prim $primOp(2)
                            |^
                            |"""
                    ),
                    SemanticTest(
                        s"Wrong number of arguments for $primOp primitive (more)",
                        s"prim $primOp(2,2,2)",
                        s"""|1:1:error: primitive $primOp expects 2 arguments got 3
                           |prim $primOp(2,2,2)
                           |^
                           |"""
                    ),
                    SemanticTest(
                        s"Wrong argument type for $primOp primitive",
                        s"""prim $primOp(\"2\",2)""",
                        s"""|1:$primCol:error: expected Int, got "2" of type String
                           |prim $primOp("2",2)
                           |$primInd^
                           |"""
                    ),
                    SemanticTest(
                        s"Wrong argument type (cont) for $primOp primitive",
                        s"""prim $primOp(2,\"2\")""",
                        s"""|1:${primCol + 2}:error: expected Int, got "2" of type String
                           |prim $primOp(2,"2")
                           |$primInd  ^
                           |"""
                    ),
                    SemanticTest(
                        s"Wrong argument type and number of arguments for $primOp primitive",
                        s"""prim $primOp(2,\"2\",2)""",
                        s"""|1:1:error: primitive $primOp expects 2 arguments got 3
                           |prim $primOp(2,"2",2)
                           |^
                           |1:${primCol + 2}:error: expected Int, got "2" of type String
                           |prim $primOp(2,"2",2)
                           |$primInd  ^
                           |"""
                    )
                )
            }) ++ Primitives.allStrPrimOps.flatMap(op => {
                val primOp = op.primName
                Vector(
                    SemanticTest(
                        s"Wrong number of arguments for $primOp primitive (no args)",
                        s"prim $primOp()",
                        s"""|1:1:error: primitive $primOp expects ${op.numArgs} arguments got 0
                            |prim $primOp()
                            |^
                            |"""
                    ),
                    if (op.numArgs == 1)
                        SemanticTest(
                        s"Wrong number of arguments for $primOp primitive (one arg)",
                        s"""prim $primOp("hello", "there")""",
                        s"""|1:1:error: primitive $primOp expects ${op.numArgs} arguments got 2
                                |prim $primOp("hello", "there")
                                |^
                                |"""
                    )
                    else
                        SemanticTest(
                            s"Wrong number of arguments for $primOp primitive (one arg)",
                            s"""prim $primOp("hello")""",
                            s"""|1:1:error: primitive $primOp expects ${op.numArgs} arguments got 1
                                |prim $primOp("hello")
                                |^
                                |"""
                        )
                ) ++
                    (op match {
                        case Primitives.LENGTH =>
                            Vector(
                                SemanticTest(
                                    s"Wrong number of arguments for $primOp primitive (more)",
                                    s"""prim $primOp("hello", 3, "bob")""",
                                    s"""|1:1:error: primitive $primOp expects ${op.numArgs} arguments got 3
                                        |prim $primOp("hello", 3, "bob")
                                        |^
                                        |"""
                                ),
                                SemanticTest(
                                    s"Wrong argument type for $primOp primitive",
                                    s"""prim $primOp(2)""",
                                    s"""|1:16:error: expected String, got 2 of type Int
                                       |prim $primOp(2)
                                       |               ^
                                       |"""
                                )
                            )
                        case Primitives.SUBSTR =>
                            Vector(
                                SemanticTest(
                                    s"Wrong number of arguments for $primOp primitive (more)",
                                    s"""prim $primOp("hello", 3, "bob")""",
                                    s"""|1:1:error: primitive $primOp expects ${op.numArgs} arguments got 3
                                        |prim $primOp("hello", 3, "bob")
                                        |^
                                        |"""
                                ),
                                SemanticTest(
                                    s"Wrong argument type for $primOp primitive",
                                    s"""prim $primOp(2,2)""",
                                    s"""|1:16:error: expected String, got 2 of type Int
                                        |prim $primOp(2,2)
                                        |               ^
                                        |"""
                                ),
                                SemanticTest(
                                    s"Wrong argument type for $primOp primitive (more)",
                                    s"""prim $primOp(\"2\",\"2\")""",
                                    s"""|1:20:error: expected Int, got "2" of type String
                                        |prim $primOp("2","2")
                                        |                   ^
                                        |"""
                                )
                            )
                        case _ =>
                            Vector(
                                SemanticTest(
                                    s"Wrong number of arguments for $primOp primitive (more)",
                                    s"""prim $primOp("hello", "there", "bob")""",
                                    s"""|1:1:error: primitive $primOp expects ${op.numArgs} arguments got 3
                                        |prim $primOp("hello", "there", "bob")
                                        |^
                                        |"""
                                ),
                                SemanticTest(
                                    s"Wrong argument type for $primOp primitive",
                                    s"""prim $primOp(2,"2")""",
                                    s"""|1:16:error: expected String, got 2 of type Int
                                        |prim $primOp(2,"2")
                                        |               ^
                                        |"""
                                ),
                                SemanticTest(
                                    s"Wrong argument type for $primOp primitive (more)",
                                    s"""prim $primOp(\"2\",2)""",
                                    s"""|1:20:error: expected String, got 2 of type Int
                                        |prim $primOp("2",2)
                                        |                   ^
                                        |"""
                                )
                            )
                    })
            })

    for (aTest <- semanticTests) {
        test(aTest.name) {
            runAnalysis(aTest.expression.stripMargin) shouldBe aTest.expectedMessages.stripMargin
        }
    }

    def runAnalysis(expression : String) : String = {
        val messages =
            driver.makeast(StringSource(expression), config) match {
                case Left(ast) =>
                    val tree = new Tree[ASTNode, Program](ast)
                    val analyser = new SemanticAnalyser(tree)
                    analyser.errors
                case Right(messages) =>
                    messages
            }
        driver.messaging.formatMessages(messages)
    }

    // Subtyping

    {
        import org.bitbucket.inkytonik.kiama.util.Positions

        val analyser = new SemanticAnalyser(new Tree[ASTNode, ASTNode](Uni()))
        import analyser.{subtype, subtypes}

        def parseType(s : String) : Expression = {
            val source = new StringSource(s)
            val positions = new Positions
            val p = new CoomaParser(source, positions)
            val pr = p.pExpression(0)
            if (pr.hasValue)
                p.value(pr).asInstanceOf[Expression]
            else
                fail(p.formatParseError(pr.parseError, false))
        }

        def parseTypes(ss : Vector[String]) : Vector[Expression] =
            ss.map(parseType)

        // NOTE: these must be designed to not be sub-types of each other

        val reflSubtypeTests =
            Vector(
                "Int",
                "String",
                "{x : Int}",
                "{a : Int, b : String}",
                "{r : Int, s : { a : Int, b : String}}",
                "() Int",
                "(Int) Int",
                "(String, Int) String",
                "(Unit) Int",
                "(String) Unit",
                "({x : Int}) String",
                "(Int) {x : Int}",
                "((Int) Int) Int",
                "(Int) (Int) Int"
            )

        for (tt <- reflSubtypeTests) {
            test(s"reflexive subtype $tt") {
                val t = parseType(tt)
                subtype(t, t) shouldBe true
            }
        }

        for (tt <- reflSubtypeTests) {
            val t = parseType(tt)
            for (uu <- reflSubtypeTests) {
                if (tt != uu) {
                    val u = parseType(uu)
                    test(s"subtype $tt not <: $uu") {
                        subtype(t, u) shouldBe false
                    }
                }
            }
        }

        val onewaySubtypeTests =
            Vector(
                ("{x : Int, y : Int}", "{x : Int}"),
                ("{x : Int, y : Int}", "{y : Int}"),
                ("{x : {b : Int, a : Int}, y : Int}", "{x : {a : Int}}"),
                ("{y : Int, x : <a : Int>}", "{x : <a : Int, b : Int>}"),
                (
                    "({x : Int}, {y : String}) Int",
                    "({x : Int}, {x : Int, y : String}) Int"
                ),
                ("<x : Int>", "<x : Int, y : Int>"),
                ("<x : {b : Int, a : Int}>", "<x : {a : Int}, y : Int>"),
                ("<x : <a : Int>>", "<y : Int, x : <b : Int, a : Int>>"),
                ("(Int) {x : Int, y : Int}", "(Int) {x : Int}")
            )

        for ((tt, uu) <- onewaySubtypeTests) {
            val t = parseType(tt)
            val u = parseType(uu)
            test(s"subtype $tt <: $uu") {
                subtype(t, u) shouldBe true
            }
            test(s"subtype $uu not <: $tt") {
                subtype(u, t) shouldBe false
            }
        }

        val twowaySubtypeTests =
            Vector(
                ("{x : Int, y : String}", "{y : String, x : Int}"),
                ("{x : Int, w : Int, y : String}", "{w : Int, x : Int, y : String}"),
                ("(Int) Int", "(x : Int) Int")
            )

        for ((tt, uu) <- twowaySubtypeTests) {
            val t = parseType(tt)
            val u = parseType(uu)
            test(s"subtype $tt <: $uu") {
                subtype(t, u) shouldBe true
            }
            test(s"subtype $uu <: $tt") {
                subtype(u, t) shouldBe true
            }
        }

        test("multiple subtypes (refl)") {
            val ts = parseTypes(reflSubtypeTests)
            subtypes(ts, ts) shouldBe true
        }

        test("multiple subtypes (one way)") {
            val ts = parseTypes(onewaySubtypeTests.map(_._1))
            val us = parseTypes(onewaySubtypeTests.map(_._2))
            subtypes(ts, us) shouldBe true
        }

        test("multiple subtypes (two way)") {
            val ts = parseTypes(twowaySubtypeTests.map(_._1))
            val us = parseTypes(twowaySubtypeTests.map(_._2))
            subtypes(ts, us) shouldBe true
            subtypes(us, ts) shouldBe true
        }

        test("multiple all not subtype") {
            val ts = parseTypes(Vector("Int", "String"))
            val us = parseTypes(Vector("String", "Int"))
            subtypes(ts, us) shouldBe false
            subtypes(us, ts) shouldBe false
        }

        test("multiple some not subtype") {
            val ts = parseTypes(Vector("Int", "String", "Int"))
            val us = parseTypes(Vector("Int", "Int", "Int"))
            subtypes(ts, us) shouldBe false
            subtypes(us, ts) shouldBe false
        }
    }
}
