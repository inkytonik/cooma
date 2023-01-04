package org.bitbucket.inkytonik.cooma.test

trait ExpressionTests extends ExecutionTests {

  def test(
      name: String,
      program: String,
      expectedCompiledResult: String,
      expectedREPLType: String,
      expectedREPLVar: String = "res0"
  ): Unit = {
    super.test(s"run: $name") { implicit bc =>
      val result = runString(name, program, Seq())
      result shouldBe ""
    }
    super.test(s"run: $name: result") { implicit bc =>
      val result = runString(name, program, Seq("-r"))
      val expectedValue = expectedCompiledResult.stripMargin
      result shouldBe s"$expectedValue\n"
    }
    super.test(s"REPL: $name") { implicit bc =>
      val result = runREPLOnLine(program, Seq())
      val expectedType = expectedREPLType.stripMargin
      val expectedValue = expectedCompiledResult.stripMargin
      val expectedResult =
        s"$expectedREPLVar : $expectedType = $expectedValue\n"
      result shouldBe expectedResult
    }
  }

  def testError(
      name: String,
      program: String,
      expectedErrorMessage: String
  ): Unit =
    super.test(s"run: $name") { implicit bc =>
      val errorMessage = runString(name, program, Seq())
      errorMessage shouldBe s"$expectedErrorMessage\n"
    }

}
