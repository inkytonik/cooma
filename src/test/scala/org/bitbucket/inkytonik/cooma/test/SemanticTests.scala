package org.bitbucket.inkytonik.cooma.test

import org.bitbucket.inkytonik.cooma.CoomaParserSyntax.{ASTNode, Program}
import org.bitbucket.inkytonik.cooma.{ReferenceDriver, SemanticAnalyser}
import org.bitbucket.inkytonik.cooma.SymbolTable.preludeStaticEnv
import org.bitbucket.inkytonik.kiama.relation.Tree
import org.bitbucket.inkytonik.kiama.util.{StringSource, Tests}

trait SemanticTests extends Tests {

    val driver = new ReferenceDriver
    val config = {
        val newConfig = driver.createConfig(Seq())
        newConfig.verify()
        newConfig
    }

    def test(name : String, expression : String, expectedMessages : String) : Unit =
        test(name) {
            runAnalysis(expression.stripMargin) shouldBe expectedMessages.stripMargin
        }

    def runAnalysis(expression : String) : String = {
        val messages =
            driver.makeast(StringSource(expression), config) match {
                case Left(ast) =>
                    val tree = new Tree[ASTNode, Program](ast)
                    val env = preludeStaticEnv(config)
                    val analyser = new SemanticAnalyser(tree, env)
                    analyser.errors
                case Right(messages) =>
                    messages
            }
        driver.messaging.formatMessages(messages)
    }

}
