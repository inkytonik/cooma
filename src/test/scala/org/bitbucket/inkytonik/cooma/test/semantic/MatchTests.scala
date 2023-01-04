package org.bitbucket.inkytonik.cooma.test.semantic

import org.bitbucket.inkytonik.cooma.test.SemanticTests

class MatchTests extends SemanticTests {

  test(
    "basic match (bind)",
    "<< x = 1 >> match { case x(a) => a }",
    ""
  )

  test(
    "basic match (wildcard)",
    "<< x = 1 >> match { case x(_) => 1 }",
    ""
  )

  test(
    "basic match (wildcard not usable)",
    "<< x = 1 >> match { case x(_) => _ }",
    """|1:34:error: _ is not declared
           |<< x = 1 >> match { case x(_) => _ }
           |                                 ^
           |"""
  )

  test(
    "basic match correct type",
    """{
             def f () Int = << x = 1 >> match { case x(a) => a }
             f ()
           }""",
    ""
  )

  test(
    "match of non-variant",
    "3 match { case x(a) => a }",
    """|1:1:error: match of non-variant type Int
           |3 match { case x(a) => a }
           |^
           |"""
  )

  test(
    "basic match wrong result type",
    """{
          |  def f () String = << x = 1 >> match { case x(a) => a }
          |  f ()
          |}""",
    """|2:21:error: expected String, got << x = 1 >> match { case x(a) => a } of type Int
           |  def f () String = << x = 1 >> match { case x(a) => a }
           |                    ^
           |"""
  )

  test(
    "non-declared name in match case",
    "<< x = 1 >> match { case x(a) => y }",
    """|1:34:error: y is not declared
           |<< x = 1 >> match { case x(a) => y }
           |                                 ^
           |"""
  )

  test(
    "correct number and type of cases for match",
    """{
            def f () <<x : Int, y : Int>> = <<x = 3>>
            f () match { case x(a) => 1 case y(b) => 2 }
        }""",
    ""
  )

  test(
    "correct number of cases but wrong type for match",
    """{
          |  def f () <<x : Int, y : Int>> = <<x = 3>>
          |  f () match { case x(a) => 1 case y(b) => "hi" }
          |}""",
    """|3:3:error: case expressions must be of a common type
           |  f () match { case x(a) => 1 case y(b) => "hi" }
           |  ^
           |"""
  )

  test(
    "incorrect number of cases for match",
    """{
          |  def f () << x : Int, y : Int >> = << x = 3 >>
          |  f () match { case x(a) => 1 }
          |}""",
    """|3:16:error: expected 2 cases, got 1
           |  f () match { case x(a) => 1 }
           |               ^
           |"""
  )

  test(
    "duplicate cases for match",
    """{
          |  def f () << x : Int, y : Int >> = << x = 3 >>
          |  f () match { case x(a) => 1 case x(b) => 2 }
          |}""",
    """|3:16:error: duplicate case for variant x
           |  f () match { case x(a) => 1 case x(b) => 2 }
           |               ^
           |3:31:error: duplicate case for variant x
           |  f () match { case x(a) => 1 case x(b) => 2 }
           |                              ^
           |"""
  )

  test(
    "incorrect variant for match",
    """{
          |  def f () << x : Int, y : Int >> = << x = 3 >>
          |  f () match { case w(a) => 1 case y(b) => 2 }
          |}""",
    """|3:16:error: variant w not present in matched type << x : Int, y : Int >>
           |  f () match { case w(a) => 1 case y(b) => 2 }
           |               ^
           |"""
  )

  test(
    "cases evaluating to values of common supertype",
    """|{
       |    type T = <<
       |        A : Unit,
       |        B : Int,
       |        C : String
       |    >>
       |    val t: T = << A = { } >>
       |    t match {
       |        case A(_) => { x = "a" }
       |        case B(y) => { x = "b", y = y }
       |        case C(z) => { x = "c", z = z }
       |    }
       |}
       |""".stripMargin,
    ""
  )

  test(
    "cases evaluating to a type parameter",
    """|{
       |  def ite(t: Type, b: Boolean, l: t, r: t) t =
       |    b match {
       |      case True(_) => l
       |      case False(_) => r
       |    }
       |  { }
       |}
       |""".stripMargin,
    ""
  )

}
