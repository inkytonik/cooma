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
                "secret string can be defined",
                "{ val x : String! = \"Hello\" 1 }",
                ""
            ),
            SemanticTest(
                "secret integer can be defined",
                "{ val x : Int! = 10 1 }",
                ""
            ),
            SemanticTest(
                "secret boolean can be defined",
                "{ val x : Boolean! = true 1 }",
                ""
            ),
            SemanticTest(
                "secret unit can be defined",
                "{ val x : Unit! = {} 1}",
                ""
            ),
            SemanticTest(
                "secret record can be defined",
                "{ val x : { a : Int! }! = { a = 10 } 1 }",
                ""
            ),
            SemanticTest(
                "secret variant can be defined",
                "{ val x : < a : Int! >! = < a = 10 > 1 }",
                ""
            ),
            SemanticTest(
                "secret type can be defined",
                "{ val x : Type! = Int 1 }",
                ""
            ),
            SemanticTest(
                "secret Reader capability can be defined",
                "fun (r : Reader!) r.read()",
                ""
            ),
            SemanticTest(
                "secret ReaderWriter capability can be defined",
                "fun (rw : ReaderWriter!) rw.write(rw.read())",
                ""
            ),
            SemanticTest(
                "secret Writer capability can be defined",
                "fun (w : Writer!) w.write()",
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
