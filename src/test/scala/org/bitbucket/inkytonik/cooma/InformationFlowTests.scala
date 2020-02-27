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

import org.bitbucket.inkytonik.kiama.util.Tests

class InformationFlowTests extends Tests {

    import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
    import org.bitbucket.inkytonik.kiama.relation.Tree
    import org.bitbucket.inkytonik.kiama.util.StringSource

    val driver = new ReferenceDriver
    val config = {
        val newConfig = driver.createConfig(Seq())
        newConfig.verify()
        newConfig
    }

    case class InformationFlowTest(
        name : String,
        expression : String,
        expectedMessages : String
    )

    val informationFlowTests =
        Vector(
            // Definitions
            InformationFlowTest(
                "secret string can be defined",
                "{ val x : String! = \"Hello\" 1 }",
                ""
            ),
            InformationFlowTest(
                "secret integer can be defined",
                "{ val x : Int! = 10 1 }",
                ""
            ),
            InformationFlowTest(
                "secret boolean can be defined",
                "{ val x : Boolean! = true 1 }",
                ""
            ),
            InformationFlowTest(
                "secret unit can be defined",
                "{ val x : Unit! = {} 1}",
                ""
            ),
            InformationFlowTest(
                "secret record can be defined",
                "{ val x : { a : Int! }! = { a = 10 } 1 }",
                ""
            ),
            InformationFlowTest(
                "secret variant can be defined",
                "{ val x : < a : Int! >! = < a = 10 > 1 }",
                ""
            ),
            InformationFlowTest(
                "secret type can be defined",
                "{ val x : Type! = Int 1 }",
                ""
            ),
            // InformationFlowTest(
            //     "secret Reader capability can be defined",
            //     "fun (r : Reader!) r.read()",
            //     ""
            // ),
            // InformationFlowTest(
            //     "secret ReaderWriter capability can be defined",
            //     "fun (rw : ReaderWriter!) rw.write(rw.read())",
            //     ""
            // ),
            // InformationFlowTest(
            //     "secret Writer capability can be defined",
            //     "fun (w : Writer!) w.write()",
            //     ""
            // ),

            // Enforce secret functions i.e. can't have Int! -> Int
            InformationFlowTest(
                "function with secret return type can accept arguments of any type",
                "fun (a : String, b : String!) b",
                ""
            ),
            InformationFlowTest(
                "function with public return type cannot accept secret arguments",
                "fun (a : String, b : String!) a",
                """|1:22:error: expression must be equally or less secure then String
                   |fun (a : String, b : String!) a
                   |                     ^
                   |"""
            ),
            InformationFlowTest(
                "function def with secret return type can accept public argument",
                "{ def func(a : String) String! = a 0 }",
                ""
            ),
            InformationFlowTest(
                "function def with secret return type can accept secret argument",
                "{ def func(a : String!) String! = a 0 }",
                ""
            ),
            InformationFlowTest(
                "function def with public return type can accept public argument",
                "{ def func(a : String) String = a 0 }",
                ""
            ),
            InformationFlowTest(
                "function def with public return type cannot accept secret argument",
                "{ def func(a : String!) String = \"Hello\" 0 }",
                """|1:16:error: expression must be equally or less secure then String
                   |{ def func(a : String!) String = "Hello" 0 }
                   |               ^
                   |"""
            ),
            InformationFlowTest(
                "function def with secret return type can accept arguments of any type (multiple mixed args)",
                "{ def func(a : String, b : String!) String! = b 0 }",
                ""
            ),
            InformationFlowTest(
                "function def with public return type cannot accept secret arguments (multiple mixed args)",
                "{ def func(a : String, b : String!) String = a 0 }",
                """|1:28:error: expression must be equally or less secure then String
                   |{ def func(a : String, b : String!) String = a 0 }
                   |                           ^
                   |"""
            ),

            // Secret matches
            InformationFlowTest(
                "secret match: bind",
                "{ val x : < a : Int >! = < a = 1 > x match { case a(n) => n } }",
                ""
            ),
            InformationFlowTest(
                "secret match: wildcard",
                "{ val x : < a : Int >! = < a = 1 > x match { case a(_) => 1 } }",
                ""
            ),
            InformationFlowTest(
                "secret match: wildcard not usable",
                "{ val x : < a : Int >! = < a = 1 > x match { case a(_) => _ } }",
                """|1:59:error: _ is not declared
                   |{ val x : < a : Int >! = < a = 1 > x match { case a(_) => _ } }
                   |                                                          ^
                   |"""
            ),
            InformationFlowTest(
                "secret match: correct type",
                """{
                     val x : < a : Int >! = < a = 1>
                     def f () Int! = x match { case a(n) => n }
                     f ()
                   }""",
                ""
            ),
            InformationFlowTest(
                "secret match: match on non-variant",
                "{ val x : Int! = 3 x match { case x(a) => a } }",
                """|1:20:error: match of non-variant type Int!
                   |{ val x : Int! = 3 x match { case x(a) => a } }
                   |                   ^
                   |"""
            ),
            InformationFlowTest(
                "secret match: wrong result type",
                """{
                  |  def f () String = { val x : < a : Int >! = < a = 1 > x match { case a(n) => n } }
                  |  f ()
                  |}""",
                """|2:21:error: expected String, got { val x: < a : Int >! = < a = 1 > x match { case a(n) => n } } of type Int!
                   |  def f () String = { val x : < a : Int >! = < a = 1 > x match { case a(n) => n } }
                   |                    ^
                   |"""
            ),
            InformationFlowTest(
                "secret match: non-declared name in match case",
                "{ val x : < a : Int >! = < a = 1 > x match { case a(n) => y } }",
                """|1:59:error: y is not declared
                   |{ val x : < a : Int >! = < a = 1 > x match { case a(n) => y } }
                   |                                                          ^
                   |"""
            ),
            InformationFlowTest(
                "secret match: correct number and type of cases for match",
                """{
                    def f () <x : Int, y : Int>! = <x = 3>
                    f () match { case x(a) => 1 case y(b) => 2 }
                }""",
                ""
            ),
            InformationFlowTest(
                "secret match: correct number of cases but wrong type for match",
                """{
                  |  def f () <x : Int, y : Int>! = <x = 3>
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
            InformationFlowTest(
                "secret match: incorrect number of cases for match",
                """{
                  |  def f () < x : Int, y : Int >! = < x = 3 >
                  |  f () match { case x(a) => 1 }
                  |}""",
                """|3:16:error: expected 2 cases, got 1
                   |  f () match { case x(a) => 1 }
                   |               ^
                   |"""
            ),
            InformationFlowTest(
                "secret match: duplicate cases for match",
                """{
                  |  def f () < x : Int, y : Int >! = < x = 3 >
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
            InformationFlowTest(
                "secret match: incorrect variant for match",
                """{
                  |  def f () < x : Int, y : Int >! = < x = 3 >
                  |  f () match { case w(a) => 1 case y(b) => 2 }
                  |}""",
                """|3:16:error: variant w not present in matched type < x : Int, y : Int >!
                   |  f () match { case w(a) => 1 case y(b) => 2 }
                   |               ^
                   |"""
            ),

            // Explicit flows
            InformationFlowTest(
                "explicit flow: cannot assign secret value to public type",
                "{ val x : Int! = 10 val y : Int = x 0 }",
                """|1:35:error: expected Int, got x of type Int!
                   |{ val x : Int! = 10 val y : Int = x 0 }
                   |                                  ^
                   |"""
            ),
            InformationFlowTest(
                "explicit flow: cannot assign secret return value of function def to public type",
                "{ def sec(x : Int!) Int! = x val y : Int = sec(10) 0 }",
                """|1:44:error: expected Int, got sec(10) of type Int!
                   |{ def sec(x : Int!) Int! = x val y : Int = sec(10) 0 }
                   |                                           ^
                   |"""
            ),
            InformationFlowTest(
                "explicit flow: cannot assign secret return value of function to public type",
                "{ val x = fun(x : Int!) x val y : Int = x(10) 0 }",
                """|1:41:error: expected Int, got x(10) of type Int!
                   |{ val x = fun(x : Int!) x val y : Int = x(10) 0 }
                   |                                        ^
                   |"""
            ),
            InformationFlowTest(
                "explicit flow: cannot assign result of secret match to public type",
                "{ val x : Boolean! = true val y : Int = x match { case True(_) => 0 case False(_) => 1 } 0 }",
                """|1:41:error: expected Int, got x match { case True(_) => 0 case False(_) => 1 } of type Int!
                   |{ val x : Boolean! = true val y : Int = x match { case True(_) => 0 case False(_) => 1 } 0 }
                   |                                        ^
                   |"""
            )

        // Capabilities
        // InformationFlowTest(
        //     "secret Reader has correct return type",
        //     "{ def f (r : Reader!) String! = r.read() 0 }",
        //     ""
        // ),
        // InformationFlowTest(
        //     "secret Writer can consume public data",
        //     "{ def f (s : String, w : Writer!) Unit = w.write(s) 0 }",
        //     ""
        // ),

        // Information leaks
        // InformationFlowTest(
        //     "public variable can be classified (assigned to secret variable",
        //     "{ val x : Int! = 10 0 }",
        //     ""
        // ),
        // InformationFlowTest(
        //     "secret variable cannot be assigned to public variable (declassified)",
        //     "{ val x : Int! = 10 val y : Int = x 0 }",
        //     """|1:35:error: expected Int, got x of type Int!
        //        |{ val x : Int! = 10 val y : Int = x 0 }
        //        |                                  ^
        //        |"""
        // ),
        // InformationFlowTest(
        //     "secret Reader cannot produce public output",
        //     "{ def f (r : Reader!) String = r.read() 0 }",
        //     """|1:32:error: expected String, got r.read() of type String!
        //        |{ def f (r : Reader!) String = r.read() 0 }
        //        |                               ^
        //        |"""
        // ),
        // InformationFlowTest(
        //     "secret Reader data cannot be used by public Writer",
        //     "{ def f (r : Reader!, w : Writer) Unit = w.write(r.read()) 0 }",
        //     """|1:50:error: expected String, got r.read() of type String!
        //        |{ def f (r : Reader!, w : Writer) Unit = w.write(r.read()) 0 }
        //        |                                                 ^
        //        |"""
        // )

        // Secret record and variant checks
        // InformationFlowTest(
        //     "secret record can only contain secret fields",
        //     "{ val x : { a : Int! }! = { a = 10 } 0 }",
        //     ""
        // ),
        // InformationFlowTest(
        //     "secret record can only contain secret fields - public",
        //     "{ val x : { a : Int }! = { a = 10 } 0 }",
        //     """|1:13:error: public field a found in secret { a : Int }
        //        |{ val x : { a : Int }! = { a = 10 } 0 }
        //        |            ^
        //        |"""
        // ),
        // InformationFlowTest(
        //     "secret record can only contain secret field - multiple mixed",
        //     "{ val x : { a : Int!, b : Int }! = { a = 10, b = 11 } 0 }",
        //     """|1:23:error: public field b found in secret { a : Int!, b : Int }
        //        |{ val x : { a : Int!, b : Int }! = { a = 10, b = 11 } 0 }
        //        |                      ^
        //        |"""
        // ),
        // InformationFlowTest(
        //     "secret record can only contain secret fields - recursive public",
        //     "{ val x : { a : { b : Int }! }! = { a = { b = 10 } } 0 }",
        //     """|1:19:error: public field b found in secret { b : Int }
        //        |{ val x : { a : { b : Int }! }! = { a = { b = 10 } } 0 }
        //        |                  ^
        //        |"""
        // ),
        // InformationFlowTest(
        //     "funciton can be used in secret record if aruments and return type are secret",
        //     "{ val x : { a : (Int!) Int! }! = { a = fun(b : Int!) b } 0 }",
        //     ""
        // ),
        // InformationFlowTest(
        //     "secret record function arguments must be secret",
        //     "{ val x : { a : (Int) Int! }! = { a = fun(b : Int) b } 0 }",
        //     """|1:18:error: public argument type found in secret function
        //        |{ val x : { a : (Int) Int! }! = { a = fun(b : Int) b } 0 }
        //        |                 ^
        //        |"""
        // ),
        // InformationFlowTest(
        //     "secret record function return type must be secret",
        //     "{ val x : { a : (Int!) Int }! = { a = fun(b : Int!) 10 } 0 }",
        //     """|1:24:error: public return type found in secret function
        //        |{ val x : { a : (Int!) Int }! = { a = fun(b : Int!) 10 } 0 }
        //        |                       ^
        //        |"""
        // ),
        // InformationFlowTest(
        //     "secret variant can only contain secret options",
        //     "{ val x : < a : Int! >! = < a = 10 > 0 }",
        //     ""
        // ),
        // InformationFlowTest(
        //     "secret variant can only contain secret options - public",
        //     "{ val x : < a : Int >! = < a = 10 > 0 }",
        //     """|1:13:error: public field a found in secret < a : Int >
        //        |{ val x : < a : Int >! = < a = 10 > 0 }
        //        |            ^
        //        |"""
        // ),
        // InformationFlowTest(
        //     "secret variant can only contain secret options - multiple mixed",
        //     "{ val x : < a : Int!, b : Int >! = < a = 10 > 0 }",
        //     """|1:23:error: public field b found in secret < a : Int!, b : Int >
        //        |{ val x : < a : Int!, b : Int >! = < a = 10 > 0 }
        //        |                      ^
        //        |"""
        // ),
        // InformationFlowTest(
        //     "secret variant can only contain secret options - recursive public",
        //     "{ val x : < a : < b : Int >! >! = < a = < b = 10 > > 0 }",
        //     """|1:19:error: public field b found in secret < b : Int >
        //        |{ val x : < a : < b : Int >! >! = < a = < b = 10 > > 0 }
        //        |                  ^
        //        |"""
        // ),
        // InformationFlowTest(
        //     "function can be used in secret variant if arguments and return type are secret",
        //     "{ val x : < a : (Int!) Int! > = < a = fun(b : Int!) b > 0 }",
        //     ""
        // ),
        // InformationFlowTest(
        //     "secret variant function arguments must be secret",
        //     "{ val x : < a : (Int) Int! >! = < a = fun(b : Int) b > 0 }",
        //     """|1:18:error: public argument type found in secret function
        //        |{ val x : < a : (Int) Int! >! = < a = fun(b : Int) b > 0 }
        //        |                 ^
        //        |"""
        // ),
        // InformationFlowTest(
        //     "secret variant function return type must be secret",
        //     "{ val x : < a : (Int!) Int >! = < a = fun(b : Int!) 10 > 0 }",
        //     """|1:24:error: public return type found in secret function
        //        |{ val x : < a : (Int!) Int >! = < a = fun(b : Int!) 10 > 0 }
        //        |                       ^
        //        |"""
        // )
        )

    for (aTest <- informationFlowTests) {
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
}
