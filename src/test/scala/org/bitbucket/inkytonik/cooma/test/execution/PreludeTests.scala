package org.bitbucket.inkytonik.cooma.test.execution

import org.bitbucket.inkytonik.cooma.test.ExecutionTests
import org.bitbucket.inkytonik.kiama.util.FileSource
import org.bitbucket.inkytonik.kiama.util.IO.deleteFile

class PreludeTests extends ExecutionTests {

    val prefix = "src/test/resources/prelude"

    {
        val prelude = s"$prefix/ok-prelude.cooma"
        val staticFilename = s"${prelude}.static"
        val dynamicFilename = s"${prelude}.dynamic"
        val staticFilenameOut = s"${staticFilename}.out"
        val dynamicFilenameOut = s"${dynamicFilename}.out"
        val testFilename = s"$prefix/ok-prelude-test.cooma"
        val testFilenameOut = s"$prefix/ok-prelude-test.out"

        test(s"process ok prelude : $prelude") { implicit bc =>
            val result = runMain(prelude, Seq("-P"), Seq())
            FileSource(staticFilename).content shouldBe FileSource(staticFilenameOut).content
            FileSource(dynamicFilename).content shouldBe FileSource(dynamicFilenameOut).content
            result shouldBe s"Wrote $staticFilename\nWrote $dynamicFilename\n"
            deleteFile(staticFilename)
            deleteFile(dynamicFilename)
        }

        test(s"use ok prelude : $testFilename") { implicit bc =>
            runMain(prelude, Seq("-P"), Seq())
            val result = runFile(testFilename, Seq("-p", prelude, "-r"), Seq(testFilename))
            result shouldBe FileSource(testFilenameOut).content
            deleteFile(staticFilename)
            deleteFile(dynamicFilename)
        }
    }

    {
        val filename = "src/test/resources/prelude/bad-syntax-prelude.cooma"
        val name = s"Prelude file ($filename)"

        test(s"process bad syntax prelude : $name") { implicit bc =>
            val result = runMain(filename, Seq("-P", "-r"), Seq())
            result shouldBe
                s"""|src/test/resources/prelude/bad-syntax-prelude.cooma:3:1:error: '=' expected
                    |}
                    |^
                    |""".stripMargin
        }
    }

    {
        val filename = "src/test/resources/prelude/bad-semantics-prelude.cooma"
        val name = s"Prelude file ($filename)"

        test(s"process bad semantics prelude : $name") { implicit bc =>
            val result = runMain(filename, Seq("-P", "-r"), Seq())
            result shouldBe
                s"""|src/test/resources/prelude/bad-semantics-prelude.cooma:3:7:error: expected Int, got "hello" of type String
                    |    f("hello")
                    |      ^
                    |""".stripMargin
        }
    }

    {
        val prelude = s"$prefix/bad-eval-prelude.cooma"
        val staticFilename = s"${prelude}.static"
        val dynamicFilename = s"${prelude}.dynamic"
        val testFilename = s"$prefix/bad-eval-prelude-test.cooma"
        val testFilenameOut = s"$prefix/bad-eval-prelude-test.out"

        test(s"process bad eval prelude : $prelude") { implicit bc =>
            val result = runMain(prelude, Seq("-P", "-r"), Seq())
            result shouldBe s"Wrote $staticFilename\nWrote $dynamicFilename\n"
            deleteFile(staticFilename)
            deleteFile(dynamicFilename)
        }

        test(s"use bad eval prelude : $testFilename") { implicit bc =>
            runMain(prelude, Seq("-P"), Seq())
            val result = runFile(testFilename, Seq("-p", prelude, "-r"), Seq(testFilename))
            result shouldBe FileSource(testFilenameOut).content
            deleteFile(staticFilename)
            deleteFile(dynamicFilename)
        }
    }

}
