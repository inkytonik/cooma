package org.bitbucket.inkytonik.cooma.test.execution

import org.bitbucket.inkytonik.cooma.test.ExecutionTests

class BooleanOperatorTests extends ExecutionTests {

  def test(
      name: String,
      filename: String,
      args: Seq[String],
      expected: Seq[String]
  ): Unit =
    super.test(name) { implicit bc =>
      val result =
        runFile(s"src/test/resources/boolean/$filename", Seq("-r"), args)
      result shouldEqual expected.mkString("", "\n", "\n")
    }

  test(
    "if-then-else lazy evaluation to then block",
    "if-then-else.cooma",
    Seq("-", ""),
    Seq("s is empty", "<< Right = {} >>")
  )

  test(
    "if-then-else lazy evaluation to else block",
    "if-then-else.cooma",
    Seq("-", "a"),
    Seq("s is non-empty", "<< Right = {} >>")
  )

  test(
    "lazy evaluation of logical operators, true * true",
    "and-or.cooma",
    Seq("-", "", ""),
    Seq(
      "checking s0",
      "checking s1",
      "checking s0",
      "both strings are empty",
      "<< Right = {} >>"
    )
  )

  test(
    "lazy evaluation of logical operators, true * false",
    "and-or.cooma",
    Seq("-", "", "a"),
    Seq(
      "checking s0",
      "checking s1",
      "checking s0",
      "one of the strings is empty",
      "<< Right = {} >>"
    )
  )

  test(
    "lazy evaluation of logical operators, false * true",
    "and-or.cooma",
    Seq("-", "a", ""),
    Seq(
      "checking s0",
      "checking s0",
      "checking s1",
      "one of the strings is empty",
      "<< Right = {} >>"
    )
  )

  test(
    "lazy evaluation of logical operators, false * false",
    "and-or.cooma",
    Seq("-", "a", "a"),
    Seq(
      "checking s0",
      "checking s0",
      "checking s1",
      "neither string is empty",
      "<< Right = {} >>"
    )
  )

  test(
    "recursive function",
    "factorial.cooma",
    Seq(),
    Seq("5040")
  )

}
