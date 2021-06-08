package org.bitbucket.inkytonik.cooma.test.execution

import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
import org.bitbucket.inkytonik.cooma.Primitives.primName
import org.bitbucket.inkytonik.cooma.Util
import org.bitbucket.inkytonik.cooma.test.ExecutionTests
import org.scalacheck.Gen
import org.scalatest.TryValues
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import wolfendale.scalacheck.regexp.RegexpGen

class PrimitiveTests extends ExecutionTests with ScalaCheckDrivenPropertyChecks with TryValues {

    // Primitive properties

    def testInt1Prim(op : UserPrimitive, f : BigInt => BigInt) : Unit =
        super.test(s"run: prim ${primName(op)}")(implicit bc =>
            forAll((i : BigInt) => runPrimTest(s"prim ${primName(op)}", i.toString, " : Int", f(i).toString)))

    def testInt2Prim(op : UserPrimitive, f : (BigInt, BigInt) => BigInt) : Unit =
        super.test(s"run: prim ${primName(op)}")(implicit bc =>
            forAll((l : BigInt, r : BigInt) => runPrimTest(s"prim ${primName(op)}", s"$l, $r", " : Int", f(l, r).toString)))

    def testIntRelPrim(op : UserPrimitive, f : (BigInt, BigInt) => Boolean) : Unit =
        super.test(s"run: prim ${primName(op)}")(implicit bc =>
            forAll((l : BigInt, r : BigInt) => runPrimTest(s"prim ${primName(op)}", s"$l, $r", " : < False : Unit, True : Unit >", f(l, r).toString)))

    testInt1Prim(IntAbsP(), _.abs)
    testInt2Prim(IntAddP(), _ + _)
    testInt2Prim(IntMulP(), _ * _)
    testInt2Prim(IntSubP(), _ - _)

    {
        val primName = "IntDiv"
        val func = (l : BigInt, r : BigInt) => l / r

        super.test(s"run: prim $primName") { implicit bc =>
            forAll { (l : BigInt, r : BigInt) =>
                whenever(r != 0) {
                    runPrimTest(s"prim $primName", s"$l, $r", " : Int", s"${func(l, r)}")
                }
            }
        }

        super.test(s"run: prim $primName by zero") { implicit bc =>
            forAll { l : BigInt =>
                val name = s"prim $primName"
                val code = s"prim $primName($l, 0)"
                val result1 = runString(name, code, Seq())
                result1 shouldBe "PrimitiveException: IntDiv: division by zero\n"
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
                        runPrimTest(s"prim $primName", s"$l, $r", " : Int", s"${func(l, r)}")
                    }
                }
            }
        }

        super.test(s"run: prim $primName (negative)") { implicit bc =>
            forAll { l : BigInt =>
                forAll(Gen.choose(-30, -1)) { r : Int =>
                    whenever(r < 0) {
                        runBadPrimTest(s"prim $primName", s"$l, $r", s"PrimitiveException: IntPow: illegal negative power $r given")
                    }
                }
            }
        }
    }

    testIntRelPrim(IntGtP(), _ > _)
    testIntRelPrim(IntGteP(), _ >= _)
    testIntRelPrim(IntLtP(), _ < _)
    testIntRelPrim(IntLteP(), _ <= _)

    {
        val primName = "Equal"

        test(s"run: Int prim $primName") { implicit bc =>
            forAll { (l : BigInt, r : BigInt) =>
                runPrimTest(s"prim $primName", s"Int, $l, $r", " : < False : Unit, True : Unit >", (l == r).toString)
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
                    runPrimTest(s"prim $primName", s""""$l", "$r"""", " : String", s""""${func(l, r)}"""")
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
                    runPrimTest(s"prim $primName", s"""String, "$l", "$r"""", " : < False : Unit, True : Unit >", func(l, r).toString)
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
                    runPrimTest(s"prim $primName", s""""$s"""", " : Int", s"${func(s)}")
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
                            runPrimTest(s"prim $primName", s""""$l", $i""", " : String", s""""${func(s, i)}"""")
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
                                s"""PrimitiveException: StrSubstr: index $i out of range for string "$l"""")
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

    def vecToLiteral(v : Vector[Int]) = v.mkString("[", ", ", "]")

    {
        val primName = "VecAppend"
        val func = (v : Vector[Int], i : Int) => v :+ i

        test(s"run: prim $primName") { implicit bc =>
            forAll { (v : Vector[Int], i : Int) =>
                val arg2 = vecToLiteral(v)
                val result = vecToLiteral(func(v, i))
                runPrimTest(s"prim $primName", s"Int, $arg2, $i", " : Vector(Int)", result)
            }
        }
    }

    {
        val primName = "VecConcat"
        val func = (v1 : Vector[Int], v2 : Vector[Int]) => v1 ++ v2

        test(s"run: prim $primName") { implicit bc =>
            forAll { (v1 : Vector[Int], v2 : Vector[Int]) =>
                val arg2 = vecToLiteral(v1)
                val arg3 = vecToLiteral(v2)
                val result = vecToLiteral(func(v1, v2))
                runPrimTest(s"prim $primName", s"Int, $arg2, $arg3", " : Vector(Int)", result)
            }
        }
    }

    {
        def indexesOf(v : Vector[Int]) : Gen[Int] =
            for {
                n <- Gen.choose(0, v.length)
            } yield n
        def notIndexesOf(v : Vector[Int]) : Gen[Int] =
            for {
                n <- Gen.oneOf(Gen.negNum[Int], Gen.posNum[Int] suchThat (i => (i < 0) || (i >= v.length)))
            } yield n

        {
            val primName = "VecGet"
            val func = (v : Vector[Int], i : Int) => v(i)

            test(s"run: prim $primName") { implicit bc =>
                forAll { (v : Vector[Int]) =>
                    whenever(!v.isEmpty) {
                        forAll(indexesOf(v)) { (i : Int) =>
                            whenever((i >= 0) && (i < v.length)) {
                                val arg2 = vecToLiteral(v)
                                runPrimTest(s"prim $primName", s"Int, $arg2, $i", " : Int", s"${func(v, i)}")
                            }
                        }
                    }
                }
            }

            test(s"run: prim $primName bad index") { implicit bc =>
                forAll { (v : Vector[Int]) =>
                    forAll(notIndexesOf(v)) { (i : Int) =>
                        whenever((i < 0) || (i >= v.length)) {
                            val arg2 = vecToLiteral(v)
                            runBadPrimTest(s"prim $primName", s"Int, $arg2, $i",
                                s"PrimitiveException: $primName: vector index out of bounds - size: ${v.length}, index: $i")
                        }
                    }
                }
            }
        }

        {
            val primName = "VecPut"
            val func = (v : Vector[Int], i : Int, e : Int) => v.updated(i, e)

            test(s"run: prim $primName") { implicit bc =>
                forAll { (v : Vector[Int]) =>
                    whenever(!v.isEmpty) {
                        forAll(indexesOf(v)) { (i : Int) =>
                            whenever((i >= 0) && (i < v.length)) {
                                forAll { (e : Int) =>
                                    val arg2 = vecToLiteral(v)
                                    val result = vecToLiteral(func(v, i, e))
                                    runPrimTest(s"prim $primName", s"Int, $arg2, $i, $e", " : Vector(Int)", result)
                                }
                            }
                        }
                    }
                }
            }

            test(s"run: prim $primName bad index") { implicit bc =>
                forAll { (v : Vector[Int]) =>
                    forAll(notIndexesOf(v)) { (i : Int) =>
                        whenever((i < 0) || (i >= v.length)) {
                            forAll { (e : Int) =>
                                val arg2 = vecToLiteral(v)
                                runBadPrimTest(s"prim $primName", s"Int, $arg2, $i, $e",
                                    s"PrimitiveException: $primName: vector index out of bounds - size: ${v.length}, index: $i")
                            }
                        }
                    }
                }
            }
        }
    }

    {
        val primName = "VecLength"
        val func = (v : Vector[Int]) => v.length

        test(s"run: prim $primName") { implicit bc =>
            forAll { (v : Vector[Int]) =>
                val arg2 = vecToLiteral(v)
                runPrimTest(s"prim $primName", s"Int, $arg2", " : Int", s"${func(v)}")
            }
        }
    }

    {
        val primName = "VecPrepend"
        val func = (v : Vector[Int], i : Int) => i +: v

        test(s"run: prim $primName") { implicit bc =>
            forAll { (v : Vector[Int], i : Int) =>
                val arg2 = vecToLiteral(v)
                val result = vecToLiteral(func(v, i))
                runPrimTest(s"prim $primName", s"Int, $arg2, $i", " : Vector(Int)", result)
            }
        }
    }

}
