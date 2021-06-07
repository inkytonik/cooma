package org.bitbucket.inkytonik.cooma.test.semantic

import org.bitbucket.inkytonik.cooma.test.SemanticTests

class VectorTests extends SemanticTests {

    test(
        "val explicit nil vector type (ok)",
        "{ val x : Vector() = [] x }",
        ""
    )

    test(
        "val explicit non-nil vector type from non-nil (ok)",
        "{ val x : Vector(Int) = [1,2,3] x }",
        ""
    )

    test(
        "val explicit non-nil vector type from nil (ok)",
        "{ val x : Vector(Int) = [] x }",
        ""
    )

    test(
        "val explicit nil vector type (bad)",
        "{ val x : Vector() = [1,2,3] x }",
        """|1:22:error: expected Vector(), got [1, 2, 3] of type Vector(Int)
           |{ val x : Vector() = [1,2,3] x }
           |                     ^
           |"""
    )

    test(
        "val explicit non-nil vector type (bad)",
        """{ val x : Vector(Int) = ["hi", "there"] x }""",
        """|1:25:error: expected Vector(Int), got ["hi", "there"] of type Vector(String)
           |{ val x : Vector(Int) = ["hi", "there"] x }
           |                        ^
           |"""
    )

    test(
        "val explicit heterogeneous vector type (ok)",
        """{ val x : Vector({a : Int}) = [{a = 1}, {a = 1, b = 2}] x }""",
        ""
    )

    test(
        "val explicit heterogeneous vector type (bad subtype)",
        """{ val x = [{a = 1, b = 2}, {a = 1}] x }""",
        """|1:12:error: all the elements in this vector must be of type { a : Int, b : Int }
           |{ val x = [{a = 1, b = 2}, {a = 1}] x }
           |           ^
           |"""
    )

    test(
        "val explicit heterogeneous vector type (bad type)",
        """{ val x = [1, "hi"] x }""",
        """|1:12:error: all the elements in this vector must be of type Int
           |{ val x = [1, "hi"] x }
           |           ^
           |"""
    )

    test(
        "bad type argument for Vector append",
        "Vectors.append(1, 2, 3)",
        """|1:16:error: expected Type, got 1 of type Int
           |Vectors.append(1, 2, 3)
           |               ^
           |1:19:error: expected Vector(1), got 2 of type Int
           |Vectors.append(1, 2, 3)
           |                  ^
           |1:22:error: expected 1, got 3 of type Int
           |Vectors.append(1, 2, 3)
           |                     ^
           |"""
    )

    test(
        "bad vector argument for Vector append",
        "Vectors.append(Int, 2, 3)",
        """|1:21:error: expected Vector(Int), got 2 of type Int
           |Vectors.append(Int, 2, 3)
           |                    ^
           |"""
    )

    test(
        "bad element argument for Vector append",
        """Vectors.append(Int, [1,2], "hi")""",
        """|1:28:error: expected Int, got "hi" of type String
           |Vectors.append(Int, [1,2], "hi")
           |                           ^
           |"""
    )

}
