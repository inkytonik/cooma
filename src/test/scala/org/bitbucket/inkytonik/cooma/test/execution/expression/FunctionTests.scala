package org.bitbucket.inkytonik.cooma.test.execution.expression

import org.bitbucket.inkytonik.cooma.test.ExpressionTests

class FunctionTests extends ExpressionTests {

  test(
    "no arguments",
    "{fun () 100}()",
    "100",
    "Int"
  )

  test(
    "unit argument",
    "{fun (x : Unit) 100}({})",
    "100",
    "Int"
  )

  test(
    "single integer argument",
    """{fun (x : Int) x}(10)""",
    "10",
    "Int"
  )

  test(
    "multiple arguments - first",
    """{fun (x : Int, y : String) x}(10, "hello")""",
    "10",
    "Int"
  )

  test(
    "multiple arguments - second",
    """{fun (x : Int, y : String) y}(10, "hello")""",
    """"hello"""",
    "String"
  )

  test(
    "multi-line function",
    """{fun (x : Int)
          x}(10)""",
    "10",
    "Int"
  )

  test(
    "record argument",
    "{fun (r : { x : Int }) r.x}({ x = 20 })",
    "20",
    "Int"
  )

  test(
    "single field record return",
    "{fun (x : Int) { a = x }}(9)",
    "{ a = 9 }",
    "{ a : Int }"
  )

  test(
    "variant argument (one)",
    "{fun (r : << x : Int >>) r}(<< x = 20 >>)",
    "<< x = 20 >>",
    "<< x : Int >>"
  )

  test(
    "function argument",
    """{fun (f : (Int) String) f(10)}(fun (x : Int) "yes")""",
    """"yes"""",
    "String"
  )

  test(
    "function return then call",
    "{fun (x : Int) fun (y : Int) x}(10)(15)",
    "10",
    "Int"
  )

  test(
    "function program result",
    "{fun (f : (Int) Int) f}(fun (x : Int) x)",
    "<function>",
    "(Int) Int"
  )

  test(
    "function with type arg",
    "{val f = fun (t : Type, x : t) x { }}",
    "{}",
    "Unit"
  )

  test(
    "partial type application",
    "{fun (t : Type, x : t) x}(Int)",
    "<function>",
    "(x : Int) Int"
  )

  test(
    "type application (fun)",
    "{fun (t : Type, x : t) x}(Int, 10)",
    "10",
    "Int"
  )

  test(
    "another type application (fun)",
    """{fun (t : Type, x : t) x}(String, "hi")""",
    """"hi"""",
    "String"
  )

  test(
    "type application at different types",
    """{
            def id(t : Type, x : t) t = x
            {
                b = id(Boolean, true),
                i = id(Int, 10),
                s = id(String, "hello"),
                r = id(Reader, { read = fun () << Right = "hello" >> })
            }
        }""",
    """{ b = << True = {} >>, i = 10, s = "hello", r = { read = <function> } }""",
    """|{
           |  b : <<
           |    False : Unit,
           |    True : Unit
           |  >>,
           |  i : Int,
           |  s : String,
           |  r : {
           |    read : () <<
           |      Left : String,
           |      Right : String
           |    >>
           |  }
           |}"""
  )

  test(
    "type application (def)",
    """{def id(t : Type, x : t) t = x id(String, "hi")}""",
    """"hi"""",
    "String"
  )

  test(
    "type application in type (def)",
    """{
             def MyOption(T : Type) Type = <<None : Unit, Some : T>>
             val x : MyOption(Int) = <<Some = 3>>
             x
        }""",
    "<< Some = 3 >>",
    "<< None : Unit, Some : Int >>"
  )

  test(
    "type application in type (val fun, single arg)",
    """{
             val MyOption = fun (T : Type) <<None : Unit, Some : T>>
             val x : MyOption(Int) = <<Some = 3>>
             x
        }""",
    "<< Some = 3 >>",
    "<< None : Unit, Some : Int >>"
  )

  test(
    "type application in type (type fun, single arg))",
    """{
             type MyOption = fun (T : Type) <<None : Unit, Some : T>>
             val x : MyOption(Int) = <<Some = 3>>
             x
        }""",
    "<< Some = 3 >>",
    "<< None : Unit, Some : Int >>"
  )

  test(
    "type application in type (prelude, single arg))",
    """{
             val x : Option(Int) = <<Some = 3>>
             x
        }""",
    "<< Some = 3 >>",
    "<< None : Unit, Some : Int >>"
  )

  test(
    "type application in type (multiple args)",
    """{
             type MyEither = fun (A : Type, B : Type) << Left : A, Right : B >>
             val x : MyEither(Int, String) = <<Left = 42>>
             x
        }""",
    "<< Left = 42 >>",
    "<< Left : Int, Right : String >>"
  )

  test(
    "type application in type (prelude, multiple args)",
    """{
             val x : Either(Int, String) = <<Right = "hi">>
             x
        }""",
    """<< Right = "hi" >>""",
    "<< Left : Int, Right : String >>"
  )

}
