package org.bitbucket.inkytonik.cooma.test.execution.expression

import org.bitbucket.inkytonik.cooma.Primitives._
import org.bitbucket.inkytonik.cooma.test.ExpressionTests

class PredefTests extends ExpressionTests {

    test(
        "true",
        "true",
        "<< True = {} >>",
        "<< False : Unit, True : Unit >>",
        "true"
    )

    test(
        "false",
        "false",
        "<< False = {} >>",
        "<< False : Unit, True : Unit >>",
        "false"
    )

    test(
        "false && false",
        "false && false",
        "<< False = {} >>",
        "<< False : Unit, True : Unit >>"
    )

    test(
        "false && true",
        "false && true",
        "<< False = {} >>",
        "<< False : Unit, True : Unit >>"
    )

    test(
        "true && false",
        "true && false",
        "<< False = {} >>",
        "<< False : Unit, True : Unit >>"
    )

    test(
        "true && true",
        "true && true",
        "<< True = {} >>",
        "<< False : Unit, True : Unit >>"
    )

    test(
        "Booleans.not(false)",
        "Booleans.not(false)",
        "<< True = {} >>",
        "<< False : Unit, True : Unit >>"
    )

    test(
        "Booleans.not(true)",
        "Booleans.not(true)",
        "<< False = {} >>",
        "<< False : Unit, True : Unit >>"
    )

    test(
        "false || false",
        "false || false",
        "<< False = {} >>",
        "<< True : Unit, False : Unit >>"
    )

    test(
        "false || true",
        "false || true",
        "<< True = {} >>",
        "<< True : Unit, False : Unit >>"
    )

    test(
        "true || false",
        "true || false",
        "<< True = {} >>",
        "<< True : Unit, False : Unit >>"
    )

    test(
        "true || true",
        "true || true",
        "<< True = {} >>",
        "<< True : Unit, False : Unit >>"
    )

    test(
        "Booleans",
        "Booleans",
        "{ not = <function> }",
        """{
          |  not : (b : <<
          |    False : Unit,
          |    True : Unit
          |  >>) <<
          |    False : Unit,
          |    True : Unit
          |  >>
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
          |  mod = <function>,
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
          |  mod : (l : Int, r : Int) Int,
          |  mul : (l : Int, r : Int) Int,
          |  pow : (l : Int, r : Int) Int,
          |  sub : (l : Int, r : Int) Int,
          |  lt : (l : Int, r : Int) <<
          |    False : Unit,
          |    True : Unit
          |  >>,
          |  lte : (l : Int, r : Int) <<
          |    False : Unit,
          |    True : Unit
          |  >>,
          |  gt : (l : Int, r : Int) <<
          |    False : Unit,
          |    True : Unit
          |  >>,
          |  gte : (l : Int, r : Int) <<
          |    False : Unit,
          |    True : Unit
          |  >>
          |}""",
        "Ints"
    )

    test(
        "<< v = Ints >>",
        "<< v = Ints >>",
        """<< v = {
          |  abs = <function>,
          |  add = <function>,
          |  div = <function>,
          |  mod = <function>,
          |  mul = <function>,
          |  pow = <function>,
          |  sub = <function>,
          |  lt = <function>,
          |  lte = <function>,
          |  gt = <function>,
          |  gte = <function>
          |} >>""",
        """<<
          |  v : {
          |    abs : (i : Int) Int,
          |    add : (l : Int, r : Int) Int,
          |    div : (l : Int, r : Int) Int,
          |    mod : (l : Int, r : Int) Int,
          |    mul : (l : Int, r : Int) Int,
          |    pow : (l : Int, r : Int) Int,
          |    sub : (l : Int, r : Int) Int,
          |    lt : (l : Int, r : Int) <<
          |      False : Unit,
          |      True : Unit
          |    >>,
          |    lte : (l : Int, r : Int) <<
          |      False : Unit,
          |      True : Unit
          |    >>,
          |    gt : (l : Int, r : Int) <<
          |      False : Unit,
          |      True : Unit
          |    >>,
          |    gte : (l : Int, r : Int) <<
          |      False : Unit,
          |      True : Unit
          |    >>
          |  }
          |>>"""
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
          |      mod = <function>,
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
          |      mod : (l : Int, r : Int) Int,
          |      mul : (l : Int, r : Int) Int,
          |      pow : (l : Int, r : Int) Int,
          |      sub : (l : Int, r : Int) Int,
          |      lt : (l : Int, r : Int) <<
          |        False : Unit,
          |        True : Unit
          |      >>,
          |      lte : (l : Int, r : Int) <<
          |        False : Unit,
          |        True : Unit
          |      >>,
          |      gt : (l : Int, r : Int) <<
          |        False : Unit,
          |        True : Unit
          |      >>,
          |      gte : (l : Int, r : Int) <<
          |        False : Unit,
          |        True : Unit
          |      >>
          |    }
          |  }
          |}"""
    )

    test(
        "equal has the correct type",
        "equal",
        "<function>",
        "(t : Type, l : t, r : t) << False : Unit, True : Unit >>",
        "equal"
    )

    test(
        "equality of integers (equal)",
        "equal(Int, 42, 42)",
        "<< True = {} >>",
        "<< False : Unit, True : Unit >>"
    )

    test(
        "equality of integers (unequal)",
        "equal(Int, 42, 99)",
        "<< False = {} >>",
        "<< False : Unit, True : Unit >>"
    )

    test(
        "equality of strings (equal)",
        s"""equal(String, "abc", "abc")""",
        "<< True = {} >>",
        "<< False : Unit, True : Unit >>"
    )

    test(
        "equality of strings (unequal)",
        s"""equal(String, "abc", "cba")""",
        "<< False = {} >>",
        "<< False : Unit, True : Unit >>"
    )

    test(
        "equality of Booleans (equal)",
        "equal(Boolean, true, true)",
        "<< True = {} >>",
        "<< False : Unit, True : Unit >>"
    )

    test(
        "equality of Booleans (unequal)",
        "equal(Boolean, true, false)",
        "<< False = {} >>",
        "<< False : Unit, True : Unit >>"
    )

    test(
        "equality of records (equal, flat)",
        "equal({x : Int, y : Int}, {x = 0, y = 1}, {y = 1, x = 0})",
        "<< True = {} >>",
        "<< False : Unit, True : Unit >>"
    )

    test(
        "equality of records (equal, nested)",
        "equal({x : { a : Int, b : Int }, y : Int}, {x = {a = 0, b = 0}, y = 1}, {y = 1, x = {b = 0, a = 0}})",
        "<< True = {} >>",
        "<< False : Unit, True : Unit >>"
    )

    test(
        "equality of records (unequal, flat)",
        "equal({x : Int, y : Int}, {x = 0, y = 0}, {y = 1, x = 0})",
        "<< False = {} >>",
        "<< False : Unit, True : Unit >>"
    )

    test(
        "equality of records (unequal, flat, extra field)",
        "equal({x : Int}, {x = 0}, {x = 0, y = 1})",
        "<< False = {} >>",
        "<< False : Unit, True : Unit >>"
    )

    test(
        "equality of records (unequal, nested)",
        "equal({x : { a : Int, b : Int }, y : Int}, {x = {a = 0, b = 0}, y = 1}, {y = 1, x = {b = 1, a = 0}})",
        "<< False = {} >>",
        "<< False : Unit, True : Unit >>"
    )

    test(
        "equality of Units (equal)",
        "equal(Unit, {}, {})",
        "<< True = {} >>",
        "<< False : Unit, True : Unit >>"
    )

    test(
        "equality of variants (equal, flat)",
        "equal(<< a : Int, v : String >>, << a = 1 >>, << a = 1 >>)",
        "<< True = {} >>",
        "<< False : Unit, True : Unit >>"
    )

    test(
        "equality of variants (equal, nested)",
        "equal(<< a : { x : Int, y : Int }, v : String >>, << a = {x = 1, y = 2} >>, << a = {y = 2, x = 1} >>)",
        "<< True = {} >>",
        "<< False : Unit, True : Unit >>"
    )

    test(
        "equality of variants (unequal, same constructor)",
        "equal(<< a : Int, v : Int >>, << a = 1 >>, << a = 2 >>)",
        "<< False = {} >>",
        "<< False : Unit, True : Unit >>"
    )

    test(
        "equality of variants (unequal, different constructor)",
        "equal(<< a : Int, v : Int >>, << a = 1 >>, << v = 2 >>)",
        "<< False = {} >>",
        "<< False : Unit, True : Unit >>"
    )

    test(
        "equality of variants (unequal, nested)",
        "equal(<< a : { x : Int, y : Int }, v : String >>, << a = {x = 1, y = 2} >>, << a = {y = 2, x = 2} >>)",
        "<< False = {} >>",
        "<< False : Unit, True : Unit >>"
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
            "(l : Int, r : Int) << False : Unit, True : Unit >>"
        )
        test(
            s"pre-defined Ints.${primFunName(op)} partial application has the correct type",
            s"Ints.${primFunName(op)}(1)",
            "<function>",
            "(r : Int) << False : Unit, True : Unit >>"
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
