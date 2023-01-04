package org.bitbucket.inkytonik.cooma.test.execution.expression

import org.bitbucket.inkytonik.cooma.test.ExpressionTests

class RecordTests extends ExpressionTests {

  test(
    "unit",
    "{}",
    "{}",
    "Unit"
  )

  test(
    "record (single int field)",
    "{ x = 65 }",
    "{ x = 65 }",
    "{ x : Int }"
  )

  test(
    "record (single string field)",
    """{ name = "Harold" }""",
    """{ name = "Harold" }""",
    "{ name : String }"
  )

  test(
    "record (two fields)",
    "{ a = 1, b = 2 }",
    "{ a = 1, b = 2 }",
    "{ a : Int, b : Int }"
  )

  test(
    "record (many fields)",
    """{ name = "Bob", age = 24, year = 1998, sex = "F" }""",
    """{ name = "Bob", age = 24, year = 1998, sex = "F" }""",
    "{ name : String, age : Int, year : Int, sex : String }"
  )

  test(
    "record (eval field)",
    "{ a = {fun (x : Int) x}(3), b = 2 }",
    "{ a = 3, b = 2 }",
    "{ a : Int, b : Int }"
  )

  test(
    "multi-line record",
    """{
            name = "Bob",
            age = 24
        }""",
    """{ name = "Bob", age = 24 }""",
    "{ name : String, age : Int }"
  )

  test(
    "field select (first of one)",
    """{ s = "Hi" }.s""",
    """"Hi"""",
    "String"
  )

  test(
    "field select (first of two)",
    """{ s = "Hi", t = 10 }.s""",
    """"Hi"""",
    "String"
  )

  test(
    "field select (second of two)",
    """{ s = "Hi", t = 10 }.t""",
    "10",
    "Int"
  )

  test(
    "field select (many fields)",
    """{ name = "Bob", age = 24, year = 1998, sex = "F" }.sex""",
    """"F"""",
    "String"
  )

  test(
    "nested field select",
    "{ r = { y = 42 } }.r.y",
    "42",
    "Int"
  )

  test(
    "record concatenation",
    """{
            val r = { x = 10, y = 20 }
            val s = { a = "Hi" }
            r & s
        }""",
    """{ x = 10, y = 20, a = "Hi" }""",
    "{ x : Int, y : Int, a : String }"
  )

  test(
    "select from record concatenation (left)",
    """{
           val r = { x = 10, y = 20 }
           val s = { a = "Hi" }
           {r & s}.x
       }""",
    "10",
    "Int"
  )

  test(
    "select from record concatenation (right)",
    """{
           val r = { x = 10, y = 20 }
           val s = { a = "Hi" }
           {r & s}.a
       }""",
    """"Hi"""",
    "String"
  )

}
