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
            // Definitions (what can and can't be defined)
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
                "secret record cannot be defined",
                "{ val x : { a : Int! }! = { a = 10 } 1 }",
                """|1:11:error: cannot have a secret { a : Int! }
                   |{ val x : { a : Int! }! = { a = 10 } 1 }
                   |          ^
                   |"""
            ),
            InformationFlowTest(
                "secret variant cannot be defined",
                "{ val x : < a : Int! >! = < a = 10 > 1 }",
                """|1:11:error: cannot have a secret < a : Int! >
                   |{ val x : < a : Int! >! = < a = 10 > 1 }
                   |          ^
                   |"""
            ),
            InformationFlowTest(
                "secret type cannot be defined",
                "{ val x : Type! = Int 1 }",
                """|1:11:error: cannot have a secret Type
                   |{ val x : Type! = Int 1 }
                   |          ^
                   |"""
            ),
            InformationFlowTest(
                "secret Reader capability can be defined",
                "fun (r : Reader!) r.read()",
                ""
            ),
            InformationFlowTest(
                "secret ReaderWriter capability can be defined",
                "fun (rw : ReaderWriter!) rw.write(rw.read())",
                ""
            ),
            InformationFlowTest(
                "secret Writer capability can be defined",
                "fun (w : Writer!) w.write()",
                ""
            ),

            // Check we can't have function violating security prop. i.e. can't have Int! -> Int
            InformationFlowTest(
                "function with secret return type can accept arguments of any type",
                "fun (a : String, b : String!) b",
                ""
            ),
            InformationFlowTest(
                "function with public return type cannot accept secret arguments",
                "fun (a : String, b : String!) a",
                """|1:1:error: security property violated, return type is less secure then one or more of the arguments
                   |fun (a : String, b : String!) a
                   |^
                   |"""
            ),
            // InformationFlowTest(
            //     "function def with secret return type can accept public argument",
            //     "{ def func(a : Int) Int! = a 0 }",
            //     ""
            // ),
            // InformationFlowTest(
            //     "function def with secret return type can accept secret argument",
            //     "{ def func(a : Int!) Int! = a 0 }",
            //     ""
            // ),
            // InformationFlowTest(
            //     "function def with secret return type can accept both public and secret arguments",
            //     "{ def func(a : Int!, b : Int) Int! = a 0 }",
            //     ""
            // ),
            // InformationFlowTest(
            //     "function def with public return type can accept public argument",
            //     "{ def func(a : Int) Int = a 0 }",
            //     ""
            // ),
            // InformationFlowTest(
            //     "function def with public return type cannot accept secret argument",
            //     "{ def func(a : Int!) Int = 0 0 }",
            //     """|1:16:error: expression must be equally or less secure then String
            //        |{ def func(a : String!) String = "Hello" 0 }
            //        |               ^
            //        |"""
            // ),
            // InformationFlowTest(
            //     "function def with secret return type can accept arguments of any type (multiple mixed args)",
            //     "{ def func(a : String, b : String!) String! = b 0 }",
            //     ""
            // ),
            // InformationFlowTest(
            //     "function def with public return type cannot accept secret arguments (multiple mixed args)",
            //     "{ def func(a : String, b : String!) String = a 0 }",
            //     """|1:28:error: expression must be equally or less secure then String
            //        |{ def func(a : String, b : String!) String = a 0 }
            //        |                           ^
            //        |"""
            // ),

            // Secret matches
            InformationFlowTest(
                "match on variant with secret field can return secret value",
                """{ val x : < a : Int! > = < a = 10 >
                    x match {
                        case a(x) => x
                    }
                }""",
                ""
            ),
            InformationFlowTest(
                "match on variant with secret field cannot return public value",
                "{ val x : < a : Int! > = < a = 10 > x match { case a(x) => 0 } }",
                """|1:37:error: security property violated, case return value is less secure then a field in the variant being matched on
                   |{ val x : < a : Int! > = < a = 10 > x match { case a(x) => 0 } }
                   |                                    ^
                   |"""
            ),
            InformationFlowTest(
                "match on variant with public field can return secret value",
                "{ def f (s : String!, v : < a : Int> ) String! = v match { case a(x) => s } f(\"Hello\", < a = 10 >) }",
                ""
            ),
            InformationFlowTest(
                "match on variant with public field can return public value",
                "{ val x : < a : Int > = < a = 10 > x match { case a(x) => x } }",
                ""
            ),
            InformationFlowTest(
                "match on variant with mixed fields can return secret value - called with public option",
                "{ def f (s : Int!, v : < a : Int, b : Int! >) Int! = v match { case a(x) => s case b(x) => s } f(0, < a = 10 >) }",
                ""
            ),
            InformationFlowTest(
                "match on variant with mixed fields can return secret value - called with secret option",
                "{ def f (s : Int!, v : < a : Int, b : Int! >) Int! = v match { case a(x) => s case b(x) => s } f(0, < b = 10 >) }",
                ""
            ),
            InformationFlowTest(
                "match on variant with mixed fields cannot return public value - called with public option",
                "{ def f (s : Int, v : < a : Int, b : Int! >) Int = v match { case a(x) => s case b(x) => s } f(0, < a = 10 >) }",
                """|1:52:error: security property violated, case return value is less secure then a field in the variant being matched on
                   |{ def f (s : Int, v : < a : Int, b : Int! >) Int = v match { case a(x) => s case b(x) => s } f(0, < a = 10 >) }
                   |                                                   ^
                   |1:52:error: security property violated, case return value is less secure then a field in the variant being matched on
                   |{ def f (s : Int, v : < a : Int, b : Int! >) Int = v match { case a(x) => s case b(x) => s } f(0, < a = 10 >) }
                   |                                                   ^
                   |"""
            ),
            InformationFlowTest(
                "match on variant with mixed fields cannot return public value - called with secret option",
                "{ def f (s : Int, v : < a : Int, b : Int! >) Int = v match { case a(x) => s case b(x) => s } f(0, < b = 10 >) }",
                """|1:52:error: security property violated, case return value is less secure then a field in the variant being matched on
                   |{ def f (s : Int, v : < a : Int, b : Int! >) Int = v match { case a(x) => s case b(x) => s } f(0, < b = 10 >) }
                   |                                                   ^
                   |1:52:error: security property violated, case return value is less secure then a field in the variant being matched on
                   |{ def f (s : Int, v : < a : Int, b : Int! >) Int = v match { case a(x) => s case b(x) => s } f(0, < b = 10 >) }
                   |                                                   ^
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

            // Capabilities
            InformationFlowTest(
                "secret Reader has correct return type",
                "{ def f (r : Reader!) String! = r.read() 0 }",
                ""
            ),
            InformationFlowTest(
                "secret Writer can consume public data",
                "{ def f (s : String, w : Writer!) Unit! = w.write(s) 0 }",
                ""
            ),

            // Information leaks
            InformationFlowTest(
                "secret Reader cannot produce public output",
                "{ def f (r : Reader!) String = r.read() 0 }",
                """|1:32:error: expected String, got r.read() of type String!
               |{ def f (r : Reader!) String = r.read() 0 }
               |                               ^
               |"""
            ),
            InformationFlowTest(
                "secret Reader data cannot be used by public Writer",
                "{ def f (r : Reader!, w : Writer) Unit = w.write(r.read()) 0 }",
                """|1:50:error: expected String, got r.read() of type String!
               |{ def f (r : Reader!, w : Writer) Unit = w.write(r.read()) 0 }
               |                                                 ^
               |"""
            )
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
