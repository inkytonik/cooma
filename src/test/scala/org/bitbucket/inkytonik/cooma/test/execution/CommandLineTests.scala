package org.bitbucket.inkytonik.cooma.test.execution

import org.bitbucket.inkytonik.cooma.test.ExecutionTests

class CommandLineTests extends ExecutionTests {

    def test(name : String, filename : String, expectedResult : String, args : Seq[String], usedArg : Int) : Unit = {
        super.test(s"run: $name ($filename)") { implicit bc =>
            val result = runFile(filename, Seq(), args)
            result shouldBe ""
        }
        super.test(s"run: $name ($filename): result") { implicit bc =>
            val result = runFile(filename, Seq("-r"), args)
            result shouldBe s"$expectedResult\n"
        }
        super.test(s"run: $name ($filename): no args") { implicit bc =>
            val result = runFile(filename, Seq(), Seq())
            result shouldBe s"PrimitiveException: ArgumentCheck: expected ${args.length} argument(s), found 0\n"
        }
    }

    test(
        "one string command argument",
        "src/test/resources/capability/stringCmdArg.cooma",
        "\"hello\"",
        Seq("hello"),
        0
    )

    test(
        "two string command arguments",
        "src/test/resources/capability/multiStringCmdArg.cooma",
        "\"there\"",
        Seq("hello", "there"),
        0
    )

}
