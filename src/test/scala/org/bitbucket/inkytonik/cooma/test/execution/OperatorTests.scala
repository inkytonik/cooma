package org.bitbucket.inkytonik.cooma.test.execution

import org.bitbucket.inkytonik.cooma.Util
import org.bitbucket.inkytonik.cooma.test.ExecutionTests
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class OperatorTests extends ExecutionTests with ScalaCheckDrivenPropertyChecks {

    val intReplType = " : Int"
    val boolReplType = " : << False : Unit, True : Unit >>"

    test(s"run: Boolean complement operator") { implicit bc =>
        forAll { (b : Boolean) =>
            runExprTest(s"!$b", boolReplType, toCoomaString(!b))
        }
    }

    test(s"run: binary Boolean operator &&") { implicit bc =>
        forAll { (l : Boolean, r : Boolean) =>
            runExprTest(s"$l && $r", boolReplType, toCoomaString(l && r))
        }
    }

    test(s"run: binary Boolean operator ||") { implicit bc =>
        forAll { (l : Boolean, r : Boolean) =>
            runExprTest(s"$l || $r", " : << True : Unit, False : Unit >>", toCoomaString(l || r))
        }
    }

    test(s"run: if-then-else") { implicit bc =>
        forAll { (b : Boolean, l : Int, r : Int) =>
            runExprTest(s"if $b then $l else $r", intReplType, (if (b) l else r).toString)
        }
    }

    test(s"run: Int absolute value operator") { implicit bc =>
        forAll { (i : BigInt) =>
            runExprTest(s"|$i|", intReplType, i.abs.toString)
        }
    }

    test(s"run: binary Int operator +") { implicit bc =>
        forAll { (l : BigInt, r : BigInt) =>
            runExprTest(s"$l - $r", intReplType, (l - r).toString)
        }
    }

    test(s"run: binary Int operator *") { implicit bc =>
        forAll { (l : BigInt, r : BigInt) =>
            runExprTest(s"$l - $r", intReplType, (l - r).toString)
        }
    }

    test(s"run: binary Int operator -") { implicit bc =>
        forAll { (l : BigInt, r : BigInt) =>
            runExprTest(s"$l - $r", intReplType, (l - r).toString)
        }
    }

    test(s"run: binary Int operator %") { implicit bc =>
        forAll { (l : BigInt, r : BigInt) =>
            whenever(r != 0) {
                runExprTest(s"$l % $r", intReplType, (l % r).toString)
            }
            val result1 = runString("mod", s"$l % 0", Seq())
            result1 shouldBe "PrimitiveException: IntMod: division by zero\n"
        }
    }

    test(s"run: binary Int operator /") { implicit bc =>
        forAll { (l : BigInt, r : BigInt) =>
            whenever(r != 0) {
                runExprTest(s"$l / $r", intReplType, (l / r).toString)
            }
            val result1 = runString("div", s"$l / 0", Seq())
            result1 shouldBe "PrimitiveException: IntDiv: division by zero\n"
        }
    }

    test(s"run: binary Int operator ** (non-negative)") { implicit bc =>
        forAll { (l : BigInt, r : BigInt) =>
            forAll(Gen.choose(0, 30)) { r : Int =>
                whenever(r >= 0) {
                    runExprTest(s"$l ** $r", intReplType, (l.pow(r)).toString)
                }
            }
        }
    }

    test(s"run: binary Int operator ** (negative)") { implicit bc =>
        forAll { (l : BigInt, r : BigInt) =>
            forAll(Gen.choose(-30, -1)) { r : Int =>
                whenever(r < 0) {
                    runBadExprTest(s"$l ** $r", s"PrimitiveException: IntPow: illegal negative power $r given")
                }
            }
        }
    }

    test(s"run: binary Int operator >") { implicit bc =>
        forAll { (l : BigInt, r : BigInt) =>
            runExprTest(s"$l > $r", boolReplType, toCoomaString(l > r))
        }
    }

    test(s"run: binary Int operator >=") { implicit bc =>
        forAll { (l : BigInt, r : BigInt) =>
            runExprTest(s"$l >= $r", boolReplType, toCoomaString(l >= r))
        }
    }

    test(s"run: binary Int operator <") { implicit bc =>
        forAll { (l : BigInt, r : BigInt) =>
            runExprTest(s"$l < $r", boolReplType, toCoomaString(l < r))
        }
    }

    test(s"run: binary Int operator <=") { implicit bc =>
        forAll { (l : BigInt, r : BigInt) =>
            runExprTest(s"$l <= $r", boolReplType, toCoomaString(l <= r))
        }
    }

    test(s"run: binary Int operator ==") { implicit bc =>
        forAll { (l : BigInt, r : BigInt) =>
            runExprTest(s"$l == $r", boolReplType, toCoomaString(l == r))
        }
    }

    test(s"run: binary Int operator !=") { implicit bc =>
        forAll { (l : BigInt, r : BigInt) =>
            runExprTest(s"$l != $r", boolReplType, toCoomaString(l != r))
        }
    }

    {
        val func = (s : String) => Util.unescape(s).length.toString

        test(s"run: String length operator") { implicit bc =>
            forAll(stringLit) { (s : String) =>
                whenever(isStringLit(s)) {
                    runExprTest(s"""|"$s"|""", intReplType, func(s))
                }
            }
        }
    }

    {
        val func = (l : String, r : String) => s""""${Util.escape(Util.unescape(l) + Util.unescape(r))}""""

        test(s"run: binary String operator ++") { implicit bc =>
            forAll(stringLit, stringLit) { (l : String, r : String) =>
                whenever(isStringLit(l) && isStringLit(r)) {
                    runExprTest(s""""$l" ++ "$r"""", " : String", func(l, r))
                }
            }
        }
    }

    {
        val func = (l : String, r : String) => toCoomaString(Util.unescape(l) == Util.unescape(r))

        test(s"run: binary String operator ==") { implicit bc =>
            forAll(stringLit, stringLit) { (l : String, r : String) =>
                whenever(isStringLit(l) && isStringLit(r)) {
                    runExprTest(s""""$l" == "$r"""", boolReplType, func(l, r))
                }
            }
        }
    }

    {
        val func = (l : String, r : String) => toCoomaString(Util.unescape(l) != Util.unescape(r))

        test(s"run: binary String operator !=") { implicit bc =>
            forAll(stringLit, stringLit) { (l : String, r : String) =>
                whenever(isStringLit(l) && isStringLit(r)) {
                    runExprTest(s""""$l" != "$r"""", boolReplType, func(l, r))
                }
            }
        }
    }

    def vecToLiteral(v : Vector[Int]) = v.mkString("[", ", ", "]")

    test(s"run: Vector length operator") { implicit bc =>
        forAll { (v : Vector[Int]) =>
            val e = vecToLiteral(v)
            runExprTest(s"|$e|", intReplType, v.length.toString)
        }
    }

    {
        val func = (l : Vector[Int], r : Vector[Int]) => l ++ r

        test(s"run: binary Vector operator ++") { implicit bc =>
            forAll { (l : Vector[Int], r : Vector[Int]) =>
                val el = vecToLiteral(l)
                val er = vecToLiteral(r)
                val result = vecToLiteral(func(l, r))
                val tipe = if (result == "[]") "Vector()" else "Vector(Int)"
                runExprTest(s"$el ++ $er", s" : $tipe", result)
            }
        }
    }
    {
        val func = (l : Vector[Int], r : Vector[Int]) => l == r

        test(s"run: binary Vector operator ==") { implicit bc =>
            forAll { (l : Vector[Int], r : Vector[Int]) =>
                val el = vecToLiteral(l)
                val er = vecToLiteral(r)
                val result = toCoomaString(func(l, r))
                runExprTest(s"$el == $er", boolReplType, result)
            }
        }
    }

    {
        val func = (l : Vector[Int], r : Vector[Int]) => l != r

        test(s"run: binary Vector operator !=") { implicit bc =>
            forAll { (l : Vector[Int], r : Vector[Int]) =>
                val el = vecToLiteral(l)
                val er = vecToLiteral(r)
                val result = toCoomaString(func(l, r))
                runExprTest(s"$el != $er", boolReplType, result)
            }
        }
    }

    def indexesOf(v : Vector[Int]) : Gen[Int] =
        for {
            n <- Gen.choose(0, v.length)
        } yield n

    {
        val func = (v : Vector[Int], i : Int) => v(i).toString

        test(s"run: binary Vector operator ! (non-empty)") { implicit bc =>
            forAll { (v : Vector[Int]) =>
                whenever(!v.isEmpty) {
                    forAll(indexesOf(v)) { (i : Int) =>
                        whenever((i >= 0) && (i < v.length)) {
                            val e = vecToLiteral(v)
                            runExprTest(s"$e!$i", intReplType, func(v, i))
                        }
                    }
                }
            }
        }
    }

    test(s"run: binary Vector operator ! (empty)") { implicit bc =>
        runBadExprTest("[]!0", "PrimitiveException: VecGet: vector index out of bounds - size: 0, index: 0")
    }

    test("if then else (single level, true)") { implicit bc =>
        runExprTest("if true then 1 else 2", intReplType, "1")
    }

    test("if then else (single level, false)") { implicit bc =>
        runExprTest("if false then 1 else 2", intReplType, "2")
    }

    test("if then else (multi-level)") { implicit bc =>
        runExprTest("if true then 0 else if true then 1 else 2", intReplType, "0")
    }

    // Precedence and associativity

    test("disjunction binds tighter than if-then-else") { implicit bc =>
        runExprTest("if true then false || true else false", " : << True : Unit, False : Unit >>", toCoomaString(true))
    }

    test("conjunction binds tighter than disjunction") { implicit bc =>
        runExprTest("true || false && true", " : << True : Unit, False : Unit >>", toCoomaString(true))
    }

    test("equality binds tighter than conjunction") { implicit bc =>
        runExprTest("2 == 3 && 3 == 4", boolReplType, toCoomaString(false))
    }

    test("relation binds tighter than equality") { implicit bc =>
        runExprTest("2 < 3 == false", boolReplType, toCoomaString(false))
    }

    test("addition binds tighter than relation") { implicit bc =>
        runExprTest("2 < 3 + 4", boolReplType, toCoomaString(true))
    }

    test("multiplication binds tighter than addition") { implicit bc =>
        runExprTest("2 + 3 * 4", intReplType, "14")
    }

    test("exponentiation binds tighter than multiplication") { implicit bc =>
        runExprTest("2 * 3 ** 4", intReplType, "162")
    }

    test("indexing binds tighter than exponentiation") { implicit bc =>
        runExprTest("[1,2,3]!2 ** 4", intReplType, "81")
    }

    test("complement binds tighter than disjunction") { implicit bc =>
        runExprTest("!true || true", " : << True : Unit, False : Unit >>", toCoomaString(true))
    }

    test("complement binds tighter than conjunction") { implicit bc =>
        runExprTest("!false && false", boolReplType, toCoomaString(false))
    }

    test("equality is left associative") { implicit bc =>
        runExprTest("4 == 3 == false", boolReplType, toCoomaString(true))
    }

    test("inequality is left associative") { implicit bc =>
        runExprTest("4 != 3 != false", boolReplType, toCoomaString(true))
    }

    test("subtraction is left associative") { implicit bc =>
        runExprTest("4 - 3 - 2", intReplType, "-1")
    }

    test("exponentiation is left associative") { implicit bc =>
        runExprTest("2 ** 3 ** 2", intReplType, "512")
    }

    test("indexing is left associative") { implicit bc =>
        runExprTest("[[1,2,3]]!0!1", intReplType, "2")
    }

    test("bracketing overrides precedence") { implicit bc =>
        runExprTest("2 * {3 + 4}", intReplType, "14")
    }

    test("bracketing overrides associativity") { implicit bc =>
        runExprTest("4 - {3 - 2}", intReplType, "3")
    }

}
