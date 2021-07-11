package org.bitbucket.inkytonik.cooma.test.semantic

import org.bitbucket.inkytonik.cooma.test.SemanticTests

class RecordTests extends SemanticTests {

    test(
        "distinct fields",
        "{ x = 1, y = 1 }",
        ""
    )

    test(
        "duplicate normal fields",
        "{ x = 1, x = 1 }",
        """|1:3:error: duplicate field x
           |{ x = 1, x = 1 }
           |  ^
           |1:10:error: duplicate field x
           |{ x = 1, x = 1 }
           |         ^
           |"""
    )

    test(
        "distinct type fields",
        "{fun (a : { x : Int, y : Int }) 0}({x = 1, y = 2})",
        ""
    )

    test(
        "duplicate type fields",
        "{fun (a : { x : Int, x : Int }) 0}({x = 1, x = 2})",
        """|1:13:error: duplicate type field x
           |{fun (a : { x : Int, x : Int }) 0}({x = 1, x = 2})
           |            ^
           |1:22:error: duplicate type field x
           |{fun (a : { x : Int, x : Int }) 0}({x = 1, x = 2})
           |                     ^
           |1:37:error: duplicate field x
           |{fun (a : { x : Int, x : Int }) 0}({x = 1, x = 2})
           |                                    ^
           |1:44:error: duplicate field x
           |{fun (a : { x : Int, x : Int }) 0}({x = 1, x = 2})
           |                                           ^
           |"""
    )

    test(
        "existent field (one)",
        "{ x = 3 }.x",
        ""
    )

    test(
        "existent field (many)",
        "{ x = 3, y = 4, z = 5 }.y",
        ""
    )

    test(
        "non-existent field (one)",
        "{ x = 3 }.y",
        """|1:11:error: y is not a field of record type { x : Int }
           |{ x = 3 }.y
           |          ^
           |"""
    )

    test(
        "non-existent field (many)",
        "{ x = 3, y = 4, z = 5 }.w",
        """|1:25:error: w is not a field of record type { x : Int, y : Int, z : Int }
           |{ x = 3, y = 4, z = 5 }.w
           |                        ^
           |"""
    )

    test(
        "selection from non-record",
        "42.x",
        """|1:4:error: selection of x field from non-record type Int
           |42.x
           |   ^
           |"""
    )

    test(
        "record concatenation (single)",
        "{ x = 1 } & { y = 2 }",
        ""
    )

    test(
        "record concatenation (multiple)",
        "{ w = 0, x =  1} & { a = 2, b = 3, c = 4 }",
        ""
    )

    test(
        "record type concatenation",
        "{ x: Int, y: String } & { z: Int }",
        ""
    )

    test(
        "bad record concatenation (left)",
        "3 & { x = 1 }",
        """|1:1:error: expected record or record type, got 3 of type Int
           |3 & { x = 1 }
           |^
           |"""
    )

    test(
        "bad record concatenation (right)",
        "{ x = 1 } & 3",
        """|1:13:error: expected record, got 3 of type Int
           |{ x = 1 } & 3
           |            ^
           |"""
    )

    test(
        "bad record concatenation (both)",
        "3 & 4",
        """|1:1:error: expected record or record type, got 3 of type Int
           |3 & 4
           |^
           |"""
    )

    test(
        "bad record concatenation (overlapping field)",
        "{ x = 1 } & { y = 1, x = 2 }",
        """|1:1:error: record concatenation has overlapping field(s) x
           |{ x = 1 } & { y = 1, x = 2 }
           |^
           |"""
    )

    test(
        "bad record concatenation (overlapping fields)",
        "{ w = 0, x = 1, y = 2 } & { y = 1, x = 2 }",
        """|1:1:error: record concatenation has overlapping field(s) x, y
           |{ w = 0, x = 1, y = 2 } & { y = 1, x = 2 }
           |^
           |"""
    )

    test(
        "bad record type concatenation (left)",
        "3 & { x: Int }",
        """|1:1:error: expected record or record type, got 3 of type Int
           |3 & { x: Int }
           |^
           |""".stripMargin
    )

    test(
        "bad record type concatenation (right)",
        "{ x: Int } & 3",
        """|1:14:error: expected record type, got 3 of type Int
           |{ x: Int } & 3
           |             ^
           |""".stripMargin
    )

    test(
        "bad record type concatenation (overlapping fields)",
        "{ x: Int, y: Int } & { x: Int, y: String, z: Int }",
        """|1:1:error: record concatenation has overlapping field(s) x, y
           |{ x: Int, y: Int } & { x: Int, y: String, z: Int }
           |^
           |""".stripMargin
    )

    test(
        "bad record concatenation (value on left, type on right)",
        "{ x = 1 } & { y: String }",
        """|1:13:error: expected record, got { y : String } of type Type
           |{ x = 1 } & { y: String }
           |            ^
           |""".stripMargin
    )

    test(
        "bad record concatenation (type on left, value on right)",
        "{ x: Int } & { y = 4 }",
        """|1:14:error: expected record type, got { y = 4 } of type { y : Int }
           |{ x: Int } & { y = 4 }
           |             ^
           |""".stripMargin
    )

}
