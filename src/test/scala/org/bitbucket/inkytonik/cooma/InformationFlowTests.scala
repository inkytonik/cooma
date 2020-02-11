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

            // Capabilities
            InformationFlowTest(
                "secret Reader has correct return type",
                "{ def f (r : Reader!) String! = r.read() 0 }",
                ""
            ),
            InformationFlowTest(
                "secret Writer can consume public data",
                "{ def f (s : String, w : Writer!) Unit = w.write(s) 0 }",
                ""
            ),

            // Information leaks
            InformationFlowTest(
                "public variable can be classified (assigned to secret variable",
                "{ val x : Int! = 10 0 }",
                ""
            ),
            InformationFlowTest(
                "secret variable cannot be assigned to public variable (declassified)",
                "{ val x : Int! = 10 val y : Int = x 0 }",
                """|1:35:error: expected Int, got x of type Int!
                   |{ val x : Int! = 10 val y : Int = x 0 }
                   |                                  ^
                   |"""
            ),
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
            ),

            // Secret record and variant checks
            InformationFlowTest(
                "secret record can only contain secret fields",
                "{ val x : { a : Int! }! = { a = 10 } 0 }",
                ""
            ),
            InformationFlowTest(
                "secret record can only contain secret fields - public",
                "{ val x : { a : Int }! = { a = 10 } 0 }",
                """|1:13:error: public field a found in secret { a : Int }
                   |{ val x : { a : Int }! = { a = 10 } 0 }
                   |            ^
                   |"""
            ),
            InformationFlowTest(
                "secret record can only contain secret field - multiple mixed",
                "{ val x : { a : Int!, b : Int }! = { a = 10, b = 11 } 0 }",
                """|1:23:error: public field b found in secret { a : Int!, b : Int }
                   |{ val x : { a : Int!, b : Int }! = { a = 10, b = 11 } 0 }
                   |                      ^
                   |"""
            ),
            InformationFlowTest(
                "secret record can only contain secret fields - recursive public",
                "{ val x : { a : { b : Int }! }! = { a = { b = 10 } } 0 }",
                """|1:19:error: public field b found in secret { b : Int }
                   |{ val x : { a : { b : Int }! }! = { a = { b = 10 } } 0 }
                   |                  ^
                   |"""
            ),
            InformationFlowTest(
                "secret variant can only contain secret options",
                "{ val x : < a : Int! >! = < a = 10 > 0 }",
                ""
            ),
            InformationFlowTest(
                "secret variant can only contain secret options - public",
                "{ val x : < a : Int >! = < a = 10 > 0 }",
                """|1:13:error: public field a found in secret < a : Int >
                   |{ val x : < a : Int >! = < a = 10 > 0 }
                   |            ^
                   |"""
            ),
            InformationFlowTest(
                "secret variant can only contain secret options - multiple mixed",
                "{ val x : < a : Int!, b : Int >! = < a = 10 > 0 }",
                """|1:23:error: public field b found in secret < a : Int!, b : Int >
                   |{ val x : < a : Int!, b : Int >! = < a = 10 > 0 }
                   |                      ^
                   |"""
            ),
            InformationFlowTest(
                "secret variant can only contain secret options - recursive public",
                "{ val x : < a : < b : Int >! >! = < a = < b = 10 > > 0 }",
                """|1:19:error: public field b found in secret < b : Int >
                   |{ val x : < a : < b : Int >! >! = < a = < b = 10 > > 0 }
                   |                  ^
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
