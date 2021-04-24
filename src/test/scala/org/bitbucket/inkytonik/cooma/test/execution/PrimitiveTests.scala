package org.bitbucket.inkytonik.cooma.test.execution

import org.bitbucket.inkytonik.cooma.Primitives._
import org.bitbucket.inkytonik.cooma.Util
import org.bitbucket.inkytonik.cooma.test.ExecutionTests
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import wolfendale.scalacheck.regexp.RegexpGen

class PrimitiveTests extends ExecutionTests with ScalaCheckDrivenPropertyChecks {

    // Primitive properties

    def testInt1Prim(op : PrimOp, f : BigInt => BigInt) : Unit =
        super.test(s"run: prim ${op.primName}")(implicit bc =>
            forAll((i : BigInt) => runPrimTest(s"prim ${op.primName}", i.toString, "Int", f(i).toString)))

    def testInt2Prim(op : PrimOp, f : (BigInt, BigInt) => BigInt) : Unit =
        super.test(s"run: prim ${op.primName}")(implicit bc =>
            forAll((l : BigInt, r : BigInt) => runPrimTest(s"prim ${op.primName}", s"$l, $r", "Int", f(l, r).toString)))

    def testIntRelPrim(op : PrimOp, f : (BigInt, BigInt) => Boolean) : Unit =
        super.test(s"run: prim ${op.primName}")(implicit bc =>
            forAll((l : BigInt, r : BigInt) => runPrimTest(s"prim ${op.primName}", s"$l, $r", "Boolean", f(l, r).toString)))

    testInt1Prim(ABS, _.abs)
    testInt2Prim(ADD, _ + _)
    testInt2Prim(MUL, _ * _)
    testInt2Prim(SUB, _ - _)

    {
        val primName = "IntDiv"
        val func = (l : BigInt, r : BigInt) => l / r

        super.test(s"run: prim $primName") { implicit bc =>
            forAll { (l : BigInt, r : BigInt) =>
                whenever(r != 0) {
                    runPrimTest(s"prim $primName", s"$l, $r", "Int", s"${func(l, r)}")
                }
            }
        }

        super.test(s"run: prim $primName by zero") { implicit bc =>
            forAll { l : BigInt =>
                val name = s"prim $primName"
                val code = s"prim $primName($l, 0)"
                val result1 = runString(name, code, Seq())
                result1 shouldBe "cooma: Error executing integer div: BigInteger divide by zero\n"
            }
        }
    }

    {
        val primName = "IntPow"
        val func = (l : BigInt, r : Int) => l.pow(r)

        super.test(s"run: prim $primName (non-negative)") { implicit bc =>
            forAll { l : BigInt =>
                forAll(Gen.choose(0, 30)) { r : Int =>
                    whenever(r >= 0) {
                        runPrimTest(s"prim $primName", s"$l, $r", "Int", s"${func(l, r)}")
                    }
                }
            }
        }

        super.test(s"run: prim $primName (negative)") { implicit bc =>
            forAll { l : BigInt =>
                forAll(Gen.choose(-30, -1)) { r : Int =>
                    whenever(r < 0) {
                        runBadPrimTest(s"prim $primName", s"$l, $r", s"cooma: IntPow: illegal negative power $r given")
                    }
                }
            }
        }
    }

    testIntRelPrim(GT, _ > _)
    testIntRelPrim(GTE, _ >= _)
    testIntRelPrim(LT, _ < _)
    testIntRelPrim(LTE, _ <= _)

    {
        val primName = "Equal"

        test(s"run: Int prim $primName") { implicit bc =>
            forAll { (l : BigInt, r : BigInt) =>
                runPrimTest(s"prim $primName", s"Int, $l, $r", "Boolean", (l == r).toString)
            }
        }
    }

    val stringLitREStr = """((\\([btnfr]|\\|"))|\w| ){0,40}"""
    val stringLitRE = stringLitREStr.r
    def isStringLit(s : String) = stringLitRE.matches(s)
    val stringLit : Gen[String] = RegexpGen.from(stringLitREStr)

    {
        val primName = "StrConcat"
        val func = (l : String, r : String) => Util.escape(Util.unescape(l) + Util.unescape(r))

        test(s"run: prim $primName") { implicit bc =>
            forAll(stringLit, stringLit) { (l : String, r : String) =>
                whenever(isStringLit(l) && isStringLit(r)) {
                    runPrimTest(s"prim $primName", s""""$l", "$r"""", "String", s""""${func(l, r)}"""")
                }
            }
        }
    }

    {
        val primName = "Equal"
        val func = (l : String, r : String) => Util.unescape(l) == Util.unescape(r)

        test(s"run: String prim $primName") { implicit bc =>
            forAll(stringLit, stringLit) { (l : String, r : String) =>
                whenever(isStringLit(l) && isStringLit(r)) {
                    runPrimTest(s"prim $primName", s"""String, "$l", "$r"""", "Boolean", func(l, r).toString)
                }
            }
        }
    }

    {
        val primName = "StrLength"
        val func = (s : String) => Util.unescape(s).length

        test(s"run: prim $primName") { implicit bc =>
            forAll(stringLit) { s : String =>
                whenever(isStringLit(s)) {
                    runPrimTest(s"prim $primName", s""""$s"""", "Int", s"${func(s)}")
                }
            }
        }
    }

    {
        val primName = "StrSubstr"
        val func = (s : String, i : BigInt) => Util.escape(s.substring(i.toInt))
        def indexesOf(s : String) : Gen[Int] =
            for {
                n <- Gen.choose(0, s.length)
            } yield n
        def notIndexesOf(s : String) : Gen[Int] =
            for {
                n <- Gen.oneOf(Gen.negNum[Int], Gen.posNum[Int] suchThat (_ > s.length))
            } yield n

        test(s"run: prim $primName") { implicit bc =>
            forAll(stringLit) { l : String =>
                whenever(isStringLit(l)) {
                    val s = Util.unescape(l)
                    forAll(indexesOf(s)) { i : Int =>
                        whenever((i >= 0) && (i <= s.length)) {
                            runPrimTest(s"prim $primName", s""""$l", $i""", "String", s""""${func(s, i)}"""")
                        }
                    }
                }
            }
        }

        test(s"run: prim $primName bad index") { implicit bc =>
            forAll(stringLit) { l : String =>
                whenever(isStringLit(l)) {
                    val s = Util.unescape(l)
                    forAll(notIndexesOf(s)) { i : Int =>
                        whenever((i < 0) || (i > s.length)) {
                            runBadPrimTest(s"prim $primName", s""""$l", $i""",
                                s"""cooma: StrSubstr: index $i out of range for string "$s"""")
                        }
                    }
                }
            }
        }

    }

    {
        val filename = "src/test/resources/primitives/intAdd.cooma"
        val name = s"Primitives file execution($filename)"
        val expectedResult = "0\n"

        test(s"run: $name") { implicit bc =>
            val result = runFile(filename, Seq(), Seq())
            result shouldBe ""
        }

        test(s"run: $name result") { implicit bc =>
            val result = runFile(filename, Seq("-r"), Seq())
            result shouldBe expectedResult
        }
    }

}
