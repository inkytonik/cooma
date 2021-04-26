package org.bitbucket.inkytonik.cooma.test.execution.expression

import org.bitbucket.inkytonik.cooma.Primitives._
import org.bitbucket.inkytonik.cooma.test.ExpressionTests

class PredefTests extends ExpressionTests {

    test(
        "true",
        "true",
        "true",
        "Boolean",
        "true"
    )

    test(
        "false",
        "false",
        "false",
        "Boolean",
        "false"
    )

    test(
        "Booleans.and(false, false)",
        "Booleans.and(false, false)",
        "false",
        "Boolean"
    )

    test(
        "Booleans.and(false, true)",
        "Booleans.and(false, true)",
        "false",
        "Boolean"
    )

    test(
        "Booleans.and(true, false)",
        "Booleans.and(true, false)",
        "false",
        "Boolean"
    )

    test(
        "Booleans.and(true, true)",
        "Booleans.and(true, true)",
        "true",
        "Boolean"
    )

    test(
        "Booleans.not(false)",
        "Booleans.not(false)",
        "true",
        "Boolean"
    )

    test(
        "Booleans.not(true)",
        "Booleans.not(true)",
        "false",
        "Boolean"
    )

    test(
        "Booleans.or(false, false)",
        "Booleans.or(false, false)",
        "false",
        "Boolean"
    )

    test(
        "Booleans.or(false, true)",
        "Booleans.or(false, true)",
        "true",
        "Boolean"
    )

    test(
        "Booleans.or(true, false)",
        "Booleans.or(true, false)",
        "true",
        "Boolean"
    )

    test(
        "Booleans.or(true, true)",
        "Booleans.or(true, true)",
        "true",
        "Boolean"
    )

    test(
        "Booleans",
        "Booleans",
        "{ and = <function>, not = <function>, or = <function> }",
        """{
          |  and : (l : Boolean, r : Boolean) Boolean,
          |  not : (b : Boolean) Boolean,
          |  or : (l : Boolean, r : Boolean) Boolean
          |}""",
        "Booleans"
    )

    test(
        "Ints",
        "Ints",
        """{
          |  abs = <function>,
          |  add = <function>,
          |  div = <function>,
          |  mul = <function>,
          |  pow = <function>,
          |  sub = <function>,
          |  lt = <function>,
          |  lte = <function>,
          |  gt = <function>,
          |  gte = <function>
          |}""",
        """{
          |  abs : (i : Int) Int,
          |  add : (l : Int, r : Int) Int,
          |  div : (l : Int, r : Int) Int,
          |  mul : (l : Int, r : Int) Int,
          |  pow : (l : Int, r : Int) Int,
          |  sub : (l : Int, r : Int) Int,
          |  lt : (l : Int, r : Int) Boolean,
          |  lte : (l : Int, r : Int) Boolean,
          |  gt : (l : Int, r : Int) Boolean,
          |  gte : (l : Int, r : Int) Boolean
          |}""",
        "Ints"
    )

    test(
        "< v = Ints >",
        "< v = Ints >",
        """< v = {
          |  abs = <function>,
          |  add = <function>,
          |  div = <function>,
          |  mul = <function>,
          |  pow = <function>,
          |  sub = <function>,
          |  lt = <function>,
          |  lte = <function>,
          |  gt = <function>,
          |  gte = <function>
          |} >""",
        """<
          |  v : {
          |    abs : (i : Int) Int,
          |    add : (l : Int, r : Int) Int,
          |    div : (l : Int, r : Int) Int,
          |    mul : (l : Int, r : Int) Int,
          |    pow : (l : Int, r : Int) Int,
          |    sub : (l : Int, r : Int) Int,
          |    lt : (l : Int, r : Int) Boolean,
          |    lte : (l : Int, r : Int) Boolean,
          |    gt : (l : Int, r : Int) Boolean,
          |    gte : (l : Int, r : Int) Boolean
          |  }
          |>"""
    )

    test(
        "{ x = { a = 1, b = Ints } }",
        "{ x = { a = 1, b = Ints } }",
        """{
          |  x = {
          |    a = 1,
          |    b = {
          |      abs = <function>,
          |      add = <function>,
          |      div = <function>,
          |      mul = <function>,
          |      pow = <function>,
          |      sub = <function>,
          |      lt = <function>,
          |      lte = <function>,
          |      gt = <function>,
          |      gte = <function>
          |    }
          |  }
          |}""",
        """{
          |  x : {
          |    a : Int,
          |    b : {
          |      abs : (i : Int) Int,
          |      add : (l : Int, r : Int) Int,
          |      div : (l : Int, r : Int) Int,
          |      mul : (l : Int, r : Int) Int,
          |      pow : (l : Int, r : Int) Int,
          |      sub : (l : Int, r : Int) Int,
          |      lt : (l : Int, r : Int) Boolean,
          |      lte : (l : Int, r : Int) Boolean,
          |      gt : (l : Int, r : Int) Boolean,
          |      gte : (l : Int, r : Int) Boolean
          |    }
          |  }
          |}"""
    )

    test(
        "equal has the correct type",
        "equal",
        "<function>",
        "(t : Type, l : t, r : t) Boolean",
        "equal"
    )

    test(
        "equality of integers (equal)",
        "equal(Int, 42, 42)",
        "true",
        "Boolean"
    )

    test(
        "equality of integers (unequal)",
        "equal(Int, 42, 99)",
        "false",
        "Boolean"
    )

    test(
        "equality of strings (equal)",
        s"""equal(String, "abc", "abc")""",
        "true",
        "Boolean"
    )

    test(
        "equality of strings (unequal)",
        s"""equal(String, "abc", "cba")""",
        "false",
        "Boolean"
    )

    test(
        "equality of Booleans (equal)",
        "equal(Boolean, true, true)",
        "true",
        "Boolean"
    )

    test(
        "equality of Booleans (unequal)",
        "equal(Boolean, true, false)",
        "false",
        "Boolean"
    )

    test(
        "equality of records (equal, flat)",
        "equal({x : Int, y : Int}, {x = 0, y = 1}, {y = 1, x = 0})",
        "true",
        "Boolean"
    )

    test(
        "equality of records (equal, nested)",
        "equal({x : { a : Int, b : Int }, y : Int}, {x = {a = 0, b = 0}, y = 1}, {y = 1, x = {b = 0, a = 0}})",
        "true",
        "Boolean"
    )

    test(
        "equality of records (unequal, flat",
        "equal({x : Int, y : Int}, {x = 0, y = 0}, {y = 1, x = 0})",
        "false",
        "Boolean"
    )

    test(
        "equality of records (unequal, nested)",
        "equal({x : { a : Int, b : Int }, y : Int}, {x = {a = 0, b = 0}, y = 1}, {y = 1, x = {b = 1, a = 0}})",
        "false",
        "Boolean"
    )

    test(
        "equality of Units (equal)",
        "equal(Unit, {}, {})",
        "true",
        "Boolean"
    )

    test(
        "equality of variants (equal, flat)",
        "equal(< a : Int, v : String >, < a = 1 >, < a = 1 >)",
        "true",
        "Boolean"
    )

    test(
        "equality of variants (equal, nested)",
        "equal(< a : { x : Int, y : Int }, v : String >, < a = {x = 1, y = 2} >, < a = {y = 2, x = 1} >)",
        "true",
        "Boolean"
    )

    test(
        "equality of variants (unequal, same constructor)",
        "equal(< a : Int, v : Int >, < a = 1 >, < a = 2 >)",
        "false",
        "Boolean"
    )

    test(
        "equality of variants (unequal, different constructor)",
        "equal(< a : Int, v : Int >, < a = 1 >, < v = 2 >)",
        "false",
        "Boolean"
    )

    test(
        "equality of variants (unequal, nested)",
        "equal(< a : { x : Int, y : Int }, v : String >, < a = {x = 1, y = 2} >, < a = {y = 2, x = 2} >)",
        "false",
        "Boolean"
    )

    test(
        s"pre-defined Strings.concat has the correct type",
        "Strings.concat",
        "<function>",
        "(l : String, r : String) String"
    )

    test(
        s"pre-defined Strings.concat partial application has the correct type",
        """Strings.concat("hi")""",
        "<function>",
        "(r : String) String"
    )

    test(
        s"pre-defined Strings.length has the correct type",
        "Strings.length",
        "<function>",
        "(s : String) Int"
    )

    test(
        s"pre-defined Strings.substr has the correct type",
        "Strings.substr",
        "<function>",
        "(s : String, i : Int) String"
    )

    test(
        s"pre-defined Strings.substr partial application has the correct type",
        """Strings.substr("hi")""",
        "<function>",
        "(i : Int) String"
    )

    for (op <- allInt1PrimBinOps)
        test(
            s"pre-defined Ints.${primFunName(op)} has the correct type",
            s"Ints.${primFunName(op)}",
            "<function>",
            "(i : Int) Int"
        )

    for (op <- allInt2PrimBinOps) {
        test(
            s"pre-defined Ints.${primFunName(op)} has the correct type",
            s"Ints.${primFunName(op)}",
            "<function>",
            "(l : Int, r : Int) Int"
        )
        test(
            s"pre-defined Ints.${primFunName(op)} partial application has the correct type",
            s"Ints.${primFunName(op)}(1)",
            "<function>",
            "(r : Int) Int"
        )
    }

    for (op <- allIntPrimRelOps) {
        test(
            s"pre-defined Ints.${primFunName(op)} has the correct type",
            s"Ints.${primFunName(op)}",
            "<function>",
            "(l : Int, r : Int) Boolean"
        )
        test(
            s"pre-defined Ints.${primFunName(op)} partial application has the correct type",
            s"Ints.${primFunName(op)}(1)",
            "<function>",
            "(r : Int) Boolean"
        )
    }

    test(
        "redefine pre-defined type",
        "{ val Boolean = 1 Boolean }",
        "1",
        "Int"
    )

    test(
        "redefine pre-defined value",
        "{ val true = 1 true }",
        "1",
        "Int"
    )

    test(
        "redefine pre-defined function",
        "{ val equal = 1 equal }",
        "1",
        "Int"
    )

}
