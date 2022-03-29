package org.bitbucket.inkytonik.cooma.test.semantic

import org.bitbucket.inkytonik.cooma.CoomaParserSyntax.Program
import org.bitbucket.inkytonik.cooma.Desugar.desugar
import org.bitbucket.inkytonik.cooma.SymbolTable.loadPrelude
import org.bitbucket.inkytonik.cooma.test.SemanticTests
import org.bitbucket.inkytonik.cooma.{CoomaParser, TopLevelRewriter}
import org.bitbucket.inkytonik.kiama.util.Messaging.Messages
import org.bitbucket.inkytonik.kiama.util.{Positions, StringSource}
import org.scalatest.EitherValues
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should

class TopLevelRewriterTests extends AnyFunSuiteLike with should.Matchers with EitherValues {

    // check the output of the rewriter
    object TopLevelRewriterOnly {

        def compile(source : String) : Program = {
            val positions = new Positions
            val parser = new CoomaParser(StringSource(source), positions)
            val result = parser.pProgram(0)
            val program = parser.value(result).asInstanceOf[Program]
            val env = loadPrelude(s"./prelude/prelude.cooma.static").toOption.get
            desugar(program, env, positions).toOption.get
        }

        def check(source : String, f : Either[Messages, Program] => Unit) : Unit =
            f(TopLevelRewriter(compile(source)))

        def test(name : String, source : String, expected : Either[Seq[String], String]) : Unit =
            TopLevelRewriterTests.this.test(name) {
                val program = TopLevelRewriter(compile(source))
                expected match {
                    case Right(expected) => program.value shouldEqual compile(expected)
                    case Left(expected)  => program.left.value.map(_.label) shouldEqual expected.toVector
                }
            }

    }

    // check that the rewriter works with the semantic analyser
    object WithSemanticAnalysis extends SemanticTests {

        override def test(name : String, expression : String, expectedMessages : String) : Unit =
            TopLevelRewriterTests.this.test(name) {
                runAnalysis(expression.stripMargin) shouldBe expectedMessages.stripMargin
            }

    }

    def test(name : String, source : String, expected : Either[(String, Seq[String]), String]) : Unit = {
        TopLevelRewriterOnly.test(s"[TopLevelRewriter] $name", source, expected.left.map(_._2))
        WithSemanticAnalysis.test(s"[SemanticAnalyser] $name", source, expected match {
            case Right(_)           => ""
            case Left((message, _)) => message
        })
    }

    def test(name : String, source : String, message : String, messages : Seq[String]) : Unit =
        test(name, source, Left((message, messages)))

    def test(name : String, source : String, expected : String) : Unit =
        test(name, source, Right(expected))

    test("do not rewrite a program consisting of a non-block expression", "0", "0")

    test(
        "do not rewrite a program containing a top-level function",
        """fun (name: String) { val prefix = "Hello " Strings.concat(prefix, name) }""",
        """fun (name: String) { val prefix = "Hello " Strings.concat(prefix, name) }""",
    )

    test(
        "do not rewrite a program consisting of a block with a non-function return",
        """{ val x = 42 val f = fun (x: Int) x + 1 f(x) }""",
        """{ val x = 42 val f = fun (x: Int) x + 1 f(x) }""",
    )

    test(
        "reject top-level defs",
        """|{
           |    def Box (A : Type) Type = { value : A }
           |    def box (A : Type, value : A) Box(A) = { value = value }
           |    fun (s : String) box(String, s)
           |}
           |""".stripMargin,
        """|2:5:error: def not allowed here
           |    def Box (A : Type) Type = { value : A }
           |    ^
           |3:5:error: def not allowed here
           |    def box (A : Type, value : A) Box(A) = { value = value }
           |    ^
           |""".stripMargin,
        Seq("def not allowed here", "def not allowed here")
    )

    test(
        "rewrite a block containing a function",
        "{ fun () 0 }",
        "fun () { 0 }",
    )

    test(
        "rewrite a let",
        "{ type Integer = Int fun () { val x : Integer = 0 x } }",
        "fun () { type Integer = Int { val x : Integer = 0 x } }"
    )

    test(
        "rewrite a let and substitute into arguments",
        "{ type Text = String fun (text : Text) text }",
        "fun (text : String) { type Text = String text }"
    )

    test(
        "rewrite and substitute multiple lets",
        """|{
           |    type File = Writer
           |    type Text = String
           |    fun (file : File, text : Text) file.write(text)
           |}
           |""".stripMargin,
        """|fun (file : Writer, text : String) {
           |    type File = Writer
           |    type Text = String
           |    file.write(text)
           |}
           |""".stripMargin
    )

    test(
        "rewrite a let with concatenation",
        """|{
           |    type File = Reader & Writer
           |    fun (file : File) {
           |        val result = file.read()
           |        val _ = file.write("")
           |        result
           |    }
           |}
           |""".stripMargin,
        """|fun (file : Reader & Writer) {
           |    type File = Reader & Writer
           |    {
           |        val result = file.read()
           |        val _ = file.write("")
           |        result
           |    }
           |}
           |""".stripMargin
    )

    test(
        "rewrite a let with application",
        "{ type IntOption = Option(Int) fun () { } }",
        "fun () { type IntOption = Option(Int) { } }"
    )

    test(
        "rewrite a let with record and variant types",
        """|{
           |    type Data = {
           |        x : Int,
           |        y : String,
           |        z : <<
           |            A : Int,
           |            B : String
           |        >>
           |    }
           |    fun () { }
           |}
           |""".stripMargin,
        """|fun () {
           |    type Data = {
           |        x : Int,
           |        y : String,
           |        z : <<
           |            A : Int,
           |            B : String
           |        >>
           |    }
           |    { }
           |}
           |""".stripMargin
    )

    test(
        "rewrite lets with vector aliases",
        """|{
           |    type Empty = Vector()
           |    type IntVector = Vector(Int)
           |    fun () { }
           |}
           |""".stripMargin,
        """|fun () {
           |    type Empty = Vector()
           |    type IntVector = Vector(Int)
           |    { }
           |}
           |""".stripMargin
    )

    test(
        "rewrite lets that depend on other lets",
        """|{
           |    type Student = { id : Int, name : String, wam : Option(Int) }
           |    type StudentTable = Table(Student)
           |    type Db = Database({ student : StudentTable })
           |    fun (db : Db) db.student.all()
           |}
           |""".stripMargin,
        """|fun (
           |    db : Database({
           |        student : Table({
           |            id : Int,
           |            name : String,
           |            wam : Option(Int)
           |        })
           |    })
           |) {
           |    type Student = { id : Int, name : String, wam : Option(Int) }
           |    type StudentTable = Table(Student)
           |    type Db = Database({ student : StudentTable })
           |    db.student.all()
           |}
           |""".stripMargin
    )

    test(
        "reject unsupported expressions",
        "{ type Box = fun (A : Type) { value : A } fun () { } }",
        """|1:14:error: not allowed here
           |{ type Box = fun (A : Type) { value : A } fun () { } }
           |             ^
           |""".stripMargin,
        Seq("not allowed here"),
    )

}
