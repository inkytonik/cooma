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

    case class SemanticTest(
        name : String,
        expression : String,
        expectedMessages : String
    )

    val informationFlowTests =
        Vector(
            // Definitions
            SemanticTest(
                "private string can be defined",
                "{ val x : String! = \"Hello\" 1 }",
                ""
            ),
            SemanticTest(
                "private integer can be defined",
                "{ val x : Int! = 10 1}",
                ""
            ),
            // SemanticTest( Need to wait till val is working properly
            //     "private variable cannot be assigned to public variable",
            //     "{ val x : Int! = 10 val y : Int = x 1}",
            //     """|1:35:error: expected value of type Int but got Int!
            //        |{ val x : Int! = 10 val y : Int = x 1}
            //        |                                  ^
            //        |"""
            // ),
            SemanticTest(
                "public variable can be used in secret variable argument",
                "{ def iden(x : String!) String! = x iden(\"Hello\")}",
                ""
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
