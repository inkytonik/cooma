package org.bitbucket.inkytonik.cooma.test.semantic

import org.bitbucket.inkytonik.cooma.CoomaParserSyntax.{
  ASTNode,
  Expression,
  Uni
}
import org.bitbucket.inkytonik.cooma.test.SemanticTests
import org.bitbucket.inkytonik.cooma.{CoomaParser, SemanticAnalyser}
import org.bitbucket.inkytonik.kiama.relation.Tree
import org.bitbucket.inkytonik.kiama.util.{Positions, StringSource}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.collection.immutable.ListSet

class BoundTests extends SemanticTests with ScalaCheckDrivenPropertyChecks {

  val analyser = new SemanticAnalyser(new Tree[ASTNode, ASTNode](Uni()))

  def parse(s: String): Expression = {
    val source = StringSource(s)
    val positions = new Positions
    val p = new CoomaParser(source, positions)
    val pr = p.pExpression(0)
    if (pr.hasValue)
      p.value(pr).asInstanceOf[Expression]
    else
      fail(p.formatParseError(pr.parseError, false))
  }

  def parse(ss: Vector[String]): Vector[Expression] =
    ss.map(parse)

  def runTest(tipes: Vector[String], expected: Option[String]): Unit = {
    val tipesParsed = parse(tipes)
    val actual = analyser.getLub(tipesParsed)
    expected match {
      case Some(expected) =>
        val expectedParsed = parse(expected)
        actual shouldEqual analyser.Bound(expectedParsed)
      case None =>
        actual shouldNot be(a[analyser.Bound])
    }
  }

  def testLub(
      name: String,
      tipes: Vector[String],
      expected: Option[String]
  ): Unit =
    super.test(name)(runTest(tipes, expected))

  ///////////
  // BASIC //
  ///////////

  testLub(
    "Equal primitives",
    Vector("1", "2"),
    Some("Int")
  )

  testLub(
    "Unrelated primitives",
    Vector("1", "\"foo\""),
    None
  )

  testLub(
    "Primitive and non-primitive",
    Vector("\"bar\"", "{ x = 1 }"),
    None
  )

  testLub(
    "Record and variant",
    Vector("{ a = 1 }", "<< A = 1 >>"),
    None
  )

  testLub(
    "Unit and Unit",
    Vector("{}", "{}"),
    Some("Unit")
  )

  testLub(
    "Records of equal type",
    Vector("{ x = 1 }", "{ x = 2 }", "{ x = 3 }"),
    Some("{ x: Int }")
  )

  testLub(
    "Variants of equal type",
    Vector("<< Some = 1 >>", "<< Some = 2 >>", "<< Some = 3 >>"),
    Some("<< Some: Int >>")
  )

  testLub(
    "Empty vectors",
    Vector.fill(3)("[]"),
    Some("Vector()")
  )

  testLub(
    "Vectors of equal type",
    Vector("[1]", "[2, 3, 4]", "[5, 6]"),
    Some("Vector(Int)")
  )

  testLub(
    "Vectors of equal type with empty vectors",
    Vector("[1]", "[]", "[2, 3, 4]", "[5, 6]", "[]"),
    Some("Vector(Int)")
  )

  testLub(
    "Vectors of unrelated type",
    Vector("[]", "[42]", "\"qwerty\""),
    None
  )

  /////////////
  // RECORDS //
  /////////////

  testLub(
    "Equal record types",
    Vector(
      "{ foo = 1, bar = \"qwerty\" }",
      "{ foo = 2, bar = \"uiop\" }"
    ),
    Some("{ foo: Int, bar: String }")
  )

  testLub(
    "Two records with non-empty LUB",
    Vector(
      "{ foo = 1, bar = \"qwerty\", baz = 42 }",
      "{ bar = \"uiop\", baz = 100, qux = \"asdf\" }"
    ),
    Some("{ bar: String, baz: Int }")
  )

  testLub(
    "Two records with empty LUB",
    Vector(
      "{ foo = 1 }",
      "{ bar = \"qwerty\", baz = 42 }"
    ),
    None
  )

  testLub(
    "Records A >: B >: C",
    Vector(
      "{ foo = 1 }",
      "{ foo = 2, bar = \"a\" }",
      "{ foo = 3, bar = \"b\", baz = 4 }"
    ),
    Some("{ foo: Int }")
  )

  testLub(
    "Records A <: B <: C",
    Vector(
      "{ foo = 3, bar = \"b\", baz = 4 }",
      "{ foo = 2, bar = \"a\" }",
      "{ foo = 1 }"
    ),
    Some("{ foo: Int }")
  )

  testLub(
    "Preservation of record field ordering",
    Vector(
      "{ foo = 1, bar = 2, baz = 3 }",
      "{ bar = 2, baz = 3, foo = 1 }",
      "{ baz = 3, qux = 4, bar = 2, foo = 1 }"
    ),
    Some("{ foo: Int, bar: Int, baz: Int }")
  )

  //////////////
  // VARIANTS //
  //////////////

  testLub(
    "Equal variant types",
    Vector(
      "<< Some = 1 >>",
      "<< Some = 2 >>"
    ),
    Some("<< Some: Int >>")
  )

  testLub(
    "Two variants of different type",
    Vector(
      "<< Left = \"qwerty\" >>",
      "<< Right = 42 >>"
    ),
    Some("<< Left: String, Right: Int >>")
  )

  testLub(
    "Multiple variants, some overlapping",
    Vector(
      "<< None = { } >>",
      "<< Some = 1 >>",
      "<< None = { } >>",
      "<< Some = 2 >>"
    ),
    Some("<< None: Unit, Some: Int >>")
  )

  testLub(
    "Preservation of variant field ordering",
    Vector(
      "<< A = 1 >>",
      "<< B = 2 >>",
      "<< A = 1 >>",
      "<< C = 3 >>",
      "<< B = 2 >>",
      "<< A = 4 >>"
    ),
    Some("<< A: Int, B: Int, C: Int >>")
  )

  /////////////
  // NESTING //
  /////////////

  testLub(
    "Records with common field of unrelated types",
    Vector(
      "{ bar = 42 }",
      "{ bar = \"qwerty\" }"
    ),
    None
  )

  testLub(
    "Records with common fields of related types",
    Vector(
      """|{
         |    foo = 42,
         |    bar = {
         |        w = 1,
         |        x = 2,
         |        y = 3,
         |        z = 4
         |    }
         |}
         |""".stripMargin,
      """|{
         |    foo = 100,
         |    bar = {
         |        x = 1,
         |        y = 2
         |    },
         |    baz = "qwerty"
         |}
         |""".stripMargin,
      """|{
         |    foo = 332,
         |    bar = {
         |        x = 1,
         |        y = 2,
         |        z = 3
         |    },
         |    baz = "uiop"
         |}
         |""".stripMargin
    ),
    Some(
      """|{
         |    foo: Int,
         |    bar: {
         |        x: Int,
         |        y: Int
         |    }
         |}
         |""".stripMargin
    )
  )

  testLub(
    "Variants with common field of unrelated type",
    Vector(
      "<< Some = 42 >>",
      "<< Some = \"qwerty\" >>"
    ),
    None
  )

  testLub(
    "Variants with common field of related type",
    Vector(
      "<< None = { } >>",
      """|<<
         |    Some = {
         |        w = 1,
         |        x = 2,
         |        y = 3,
         |        z = 4
         |    }
         |>>
         |""".stripMargin,
      """|<<
         |    Some = {
         |        x = 1,
         |        y = 2
         |    }
         |>>
         |""".stripMargin,
      """|<<
         |    Some = {
         |        x = 1,
         |        y = 2,
         |        z = 3
         |    }
         |>>
         |""".stripMargin
    ),
    Some(
      """|<<
         |    None: Unit,
         |    Some: {
         |        x: Int,
         |        y: Int
         |    }
         |>>
         |""".stripMargin
    )
  )

  testLub(
    "Vector with elements of unrelated type",
    Vector("[42, \"qwerty\", []]"),
    None
  )

  testLub(
    "Vector with elements of related type",
    Vector("[{ a = 1 }, { a = 1, b = 2 }]"),
    Some("Vector({ a: Int })")
  )

  ///////////////
  // FUNCTIONS //
  ///////////////

  test(
    "Functions of same type",
    """|{
       |    def add_one(x: Int) Int = x + 1
       |    def add_two(x: Int) Int = x + 2
       |    val fs = [add_one, add_two]
       |    val fs_checked: Vector((Int) Int) = fs
       |    { }
       |}
       |""".stripMargin,
    ""
  )

  test(
    "Mismatching function arity",
    """|{
       |    def add_one(x: Int) Int = x + 1
       |    def add(x: Int, y: Int) Int = x + y
       |    val fs = [add_one, add]
       |    { }
       |}
       |""".stripMargin,
    """|4:14:error: Vector elements must be of a common type
       |    val fs = [add_one, add]
       |             ^
       |""".stripMargin
  )

  test(
    "Covariance in return type, A >: B >: C",
    """|{
       |    type A = { a: Int }
       |    type AB = A & { b: Int }
       |    type ABC = AB & { c: Int }
       |    def f0(x: Int) A = { a = x }
       |    def f1(x: Int) AB = { a = x, b = x }
       |    def f2(x: Int) ABC = { a = x, b = x, c = x }
       |    val fs = [f0, f1, f2]
       |    val fs_checked: Vector((Int) A) = fs
       |    { }
       |}
       |""".stripMargin,
    ""
  )

  test(
    "Covariance in return type, A <: B <: C",
    """|{
       |    type A = { a: Int }
       |    type AB = A & { b: Int }
       |    type ABC = AB & { c: Int }
       |    def f0(x: Int) A = { a = x }
       |    def f1(x: Int) AB = { a = x, b = x }
       |    def f2(x: Int) ABC = { a = x, b = x, c = x }
       |    val fs = [f2, f1, f0]
       |    val fs_checked: Vector((Int) A) = fs
       |    { }
       |}
       |""".stripMargin,
    ""
  )

  test(
    "Mixed currying",
    """|{
       |    def add_one(x: Int) Int = x + 1
       |    def add(x: Int, y: Int) Int = x + y
       |    val fs = [add_one, add(7)]
       |    val fs_checked: Vector((Int) Int) = fs
       |    { }
       |}
       |""".stripMargin,
    ""
  )

  test(
    "Contravariance, A >: B >: C",
    """|{
       |    type A = { a: Int }
       |    type AB = A & { b: Int }
       |    type ABC = AB & { c: Int }
       |    def f0(a: A) Unit = { }
       |    def f1(ab: AB) Unit = { }
       |    def f2(abc: ABC) Unit = { }
       |    val fs = [f0, f1, f2]
       |    val fs_checked: Vector((ABC) Unit) = fs
       |    { }
       |}
       |""".stripMargin,
    ""
  )

  test(
    "Contravariance, A <: B <: C",
    """|{
       |    type A = { a: Int }
       |    type AB = A & { b: Int }
       |    type ABC = AB & { c: Int }
       |    def f0(a: A) Unit = { }
       |    def f1(ab: AB) Unit = { }
       |    def f2(abc: ABC) Unit = { }
       |    val fs = [f2, f1, f0]
       |    val fs_checked: Vector((ABC) Unit) = fs
       |    { }
       |}
       |""".stripMargin,
    ""
  )

  /* We define:
   * O = { o: Int }
   * X = { o: Int, x: Int }
   * Y = { o: Int, y: Int }
   * XY = { o: Int, x: Int, y: Int }
   *
   * Thus we have O <: X, O <: Y, X <: XY, and Y <: XY.
   *
   * This test uses ScalaCheck to check the LUB calculation for pairs of function types of the
   * form A -> B (where each of A and B is O, X, Y, or XY). */
  super.test("Covariance and contravariance") {

    sealed trait Field
    case object X extends Field
    case object Y extends Field

    case class Type(fields: ListSet[Field]) {

      def x = fields(X)
      def y = fields(Y)

      lazy val name = {
        val name = fields.mkString
        if (name.isEmpty) "O" else name
      }

      lazy val tipe =
        ("o" +: fields.map(_.toString.toLowerCase).toSeq)
          .map(s => s"$s: Int")
          .mkString("{ ", ", ", " }")

      lazy val value =
        ("o" +: fields.map(_.toString.toLowerCase).toSeq)
          .map(s => s"$s = 0")
          .mkString("{ ", ", ", " }")

      def lub(other: Type) =
        Type(this.fields.intersect(other.fields))

      def glb(other: Type) =
        Type(this.fields.union(other.fields))

    }

    case class Fun(a: Type, b: Type) {

      def lub(other: Fun) =
        Fun(this.a.glb(other.a), this.b.lub(other.b))

      def value =
        s"fun (x: ${a.tipe}) ${b.value}"

      override def toString =
        s"${a.name} -> ${b.name}"

    }

    implicit val funArb: Arbitrary[Fun] =
      Arbitrary(
        Gen.listOfN(4, Arbitrary.arbBool.arbitrary).map {
          case ax :: ay :: bx :: by :: Nil =>
            def fromBools(x: Boolean, y: Boolean): Type = {
              val xls = if (x) ListSet(X) else ListSet.empty
              val yls = if (y) ListSet(Y) else ListSet.empty
              Type(xls ++ yls)
            }
            Fun(fromBools(ax, ay), fromBools(bx, by))
          case l =>
            throw new MatchError(l)
        }
      )

    forAll { (f: Fun, g: Fun) =>
      val expected = f.lub(g)
      runTest(
        Vector(
          f.value,
          g.value
        ),
        Some(s"(x: ${expected.a.tipe}) ${expected.b.tipe}")
      )
    }

  }

}
