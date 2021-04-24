package org.bitbucket.inkytonik.cooma.test.execution

import org.bitbucket.inkytonik.cooma.ReferenceFrontend
import org.bitbucket.inkytonik.cooma.test.ExecutionTests

class OptionTests extends ExecutionTests {

    val resourcesPath = "src/test/resources"

    val backend = BackendConfig("Reference", new ReferenceFrontend, Seq())

    def test(name : String, option : String, inputBasename : String, expectedExtension : String, args : Seq[String] = Seq()) : Unit = {
        val inputFilename = s"$inputBasename.cooma"
        val expectedFilename = s"$inputBasename.$expectedExtension"
        filetest(s"[${backend.name}] file", resourcesPath, s"$resourcesPath/$expectedFilename",
            backend.options ++ List(option, s"$resourcesPath/$inputFilename") ++ args,
            expectedFilename)
    }

    test("Cooma AST print", "-C", "basic/singleArgCall", "coomaAST")
    test("IR print", "-i", "basic/singleArgCall", "IR")
    test("IR AST print", "-I", "basic/singleArgCall", "IRAST")
    test("Cooma AST print", "-C", "basic/multiArgCall", "coomaAST")
    test("IR print", "-i", "basic/multiArgCall", "IR")
    test("IR AST print", "-I", "basic/multiArgCall", "IRAST")
    test("Cooma AST print", "-C", "basic/blockVal", "coomaAST")
    test("IR print", "-i", "basic/blockVal", "IR")
    test("IR AST print", "-I", "basic/blockVal", "IRAST")
    test("Cooma AST print", "-C", "basic/blockDef", "coomaAST")
    test("IR print", "-i", "basic/blockDef", "IR")
    test("IR AST print", "-I", "basic/blockDef", "IRAST")
    test("Type print", "-t", "basic/boolean", "type")
    test("Type print", "-t", "capability/readerCmdArg", "type", Seq("/dev/null"))
    test("Usage", "--usage", "basic/integer", "usage", Seq())
    test("Usage", "--usage", "capability/readerCmdArg", "usage", Seq())
    test("Cooma AST print", "-C", "capability/writerCmdArg", "coomaAST", Seq("/dev/null"))
    test("IR print", "-i", "capability/writerCmdArg", "IR", Seq("/dev/null"))
    test("IR AST print", "-I", "capability/writerCmdArg", "IRAST", Seq("/dev/null"))
    test("IR print", "-i", "capability/readerWriterCmdArg", "IR", Seq("/dev/null"))
    test("Usage", "--usage", "capability/readerWriterCmdArg", "usage", Seq("/dev/null"))
    test("IR print", "-i", "capability/httpClientCmdArg", "IR", Seq("http://localhost:8080"))
    test("Usage", "--usage", "capability/httpClientCmdArg", "usage", Seq("http://localhost:8080"))

}
