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
        "bad heterogeneous vector type (simple)",
        """[1, "hi"]""",
        """|1:1:error: vector elements must be of same type or subtypes of a common supertype
           |[1, "hi"]
           |^
           |"""
    )

    test(
        "heterogeneous vector type (record, increasing)",
        "{ val x : Vector({a : Int}) = [{a = 1}, {a = 2, b = 3}, {a = 4, b = 5, c = 6}] x }",
        ""
    )

    test(
        "heterogeneous vector type (record, decreasing)",
        "{ val x : Vector({a : Int}) = [{a = 4, b = 5, c = 6}, {a = 2, b = 3}, {a = 1}] x }",
        ""
    )

    test(
        "heterogeneous vector type (record, mixed)",
        "{ val x : Vector({a : Int}) = [{a = 4, b = 5, c = 6}, {a = 1}, {a = 2, b = 3}] x }",
        ""
    )

    test(
        "heterogeneous vector type (record, new type)",
        "{ val x : Vector({a : Int}) = [{a = 1, b = 2}, {a = 3, c = 4}] x }",
        ""
    )

    test(
        "bad heterogeneous vector type (record)",
        "[{a = 1, b = 2}, {c = 3}]",
        """|1:1:error: vector elements must be of same type or subtypes of a common supertype
           |[{a = 1, b = 2}, {c = 3}]
           |^
           |"""
    )

    test(
        "heterogeneous vector type (record, field type mismatch)",
        """{ val x : Vector({b : Int}) = [{a = 1, b = 2}, {a = "hi", b = 3}] x }""",
        ""
    )

    test(
        "bad heterogeneous vector type (record, no common fields)",
        """[{a = 1, b = 2}, {a = "hi"}]""",
        """|1:1:error: vector elements must be of same type or subtypes of a common supertype
           |[{a = 1, b = 2}, {a = "hi"}]
           |^
           |"""
    )

    test(
        "heterogeneous vector type (record, recursive)",
        """{ val x : Vector({a : {b : Int}}) = [{a = {b = 1, c = 2}}, {a = {b = 3, d = 4}}] x }""",
        ""
    )

    test(
        "heterogeneous vector type (vector, increasing)",
        "{ val x : Vector(Vector(Int)) = [[], [1,2], [], [3, 4, 5]] x }",
        ""
    )

    test(
        "heterogeneous vector type (vector, decreasing)",
        "{ val x : Vector(Vector(Int)) = [[1,2], [], [3, 4, 5]] x }",
        ""
    )

    test(
        "bad heterogeneous vector type (vector)",
        """[[1,2], ["hi"]]""",
        """|1:1:error: vector elements must be of same type or subtypes of a common supertype
           |[[1,2], ["hi"]]
           |^
           |"""
    )

    test(
        "heterogeneous vector type (vector, recursive)",
        """{ val x : Vector(Vector({a : {b : Int}})) = [[{a = {b = 1, c = 2}}], [{a = {b = 3, d = 4}}]] x }""",
        ""
    )

    test(
        "heterogeneous vector type (variant, one field)",
        "{ val x : Vector(<a : Int>) = [<a = 1>, <a = 2>] x }",
        ""
    )

    test(
        "heterogeneous vector type (variant, multiple fields)",
        "{ val x : Vector(<a : Int, b : Int, c : Int>) = [<a = 1>, <b = 2>, <a = 3>, <c = 4>] x }",
        ""
    )

    test(
        "heterogeneous vector type (variant, different fields)",
        """{ val x : Vector(<a : Int, b : String, c : Int>) = [<a = 1>, <b = "hi">, <c = 2>] x }""",
        ""
    )

    test(
        "bad heterogeneous vector type (variant, field type mismatch)",
        """[<a = 1>, <b = 2>, <b = "hi">, <c = 3>]""",
        """|1:1:error: vector elements must be of same type or subtypes of a common supertype
           |[<a = 1>, <b = 2>, <b = "hi">, <c = 3>]
           |^
           |"""
    )

    // FIXME: test common b's with joinable types

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

    test(
        "bad elemnt type for Vector",
        "Vector(3)",
        """|1:8:error: expected Type, got 3 of type Int
           |Vector(3)
           |       ^
           |"""
    )

}
