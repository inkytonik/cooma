package org.bitbucket.inkytonik.cooma.test.semantic

import org.bitbucket.inkytonik.cooma.CoomaParserSyntax.{ASTNode, Expression, Uni}
import org.bitbucket.inkytonik.cooma.{CoomaParser, SemanticAnalyser}
import org.bitbucket.inkytonik.kiama.relation.Tree
import org.bitbucket.inkytonik.kiama.util.{StringSource, Tests}

class SubtypeTests extends Tests {

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
            "(Int) (Int) Int",
            "Vector(Int)"
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
            ("{y : Int, x : <<a : Int>>}", "{x : <<a : Int, b : Int>>}"),
            (
                "({x : Int}, {y : String}) Int",
                "({x : Int}, {x : Int, y : String}) Int"
            ),
            ("<<x : Int>>", "<<x : Int, y : Int>>"),
            ("<<x : {b : Int, a : Int}>>", "<<x : {a : Int}, y : Int>>"),
            ("<<x : <<a : Int>>>>", "<<y : Int, x : <<b : Int, a : Int>>>>"),
            ("(Int) {x : Int, y : Int}", "(Int) {x : Int}"),
            ("Vector()", "Vector(Int)"),
            ("Vector({x : Int, y : Int})", "Vector({x : Int})")
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
