package org.bitbucket.inkytonik.cooma.test.execution.expression

import org.bitbucket.inkytonik.cooma.test.ExpressionTests

class VectorTests extends ExpressionTests {

    test(
        "equality of vectors (equal, nil)",
        "equal(Vector(), [], [])",
        "true",
        "< False : Unit, True : Unit >"
    )

    test(
        "equality of vectors (equal, flat)",
        "equal(Vector(Int), [1, 2, 3], [1, 2, 3])",
        "true",
        "< False : Unit, True : Unit >"
    )

    test(
        "equality of vectors (equal, nested)",
        "equal(Vector({a : Int}), [{a = 1}, {a = 2}], [{a = 1}, {a = 2}])",
        "true",
        "< False : Unit, True : Unit >"
    )

    test(
        "equality of vectors (unequal, flat)",
        "equal(Vector(Int), [1, 2, 3], [1, 2])",
        "false",
        "< False : Unit, True : Unit >"
    )

    test(
        "equality of vectors (unequal, nested)",
        "equal(Vector({a : Int}), [{a = 1}, {a = 2}], [{a = 2}, {a = 2}])",
        "false",
        "< False : Unit, True : Unit >"
    )

    test(
        s"nil vector literal",
        "[]",
        "[]",
        "Vector()"
    )

    test(
        s"non-nil vector literal (literals)",
        "[1, 2, 3]",
        "[1, 2, 3]",
        "Vector(Int)"
    )

    test(
        s"non-nil vector literal (sub-expressions)",
        "[prim IntAdd(1,2), prim IntSub(2, 1)]",
        "[3, 1]",
        "Vector(Int)"
    )

    test(
        "empty Int Vector declaration",
        """{
            val x : Vector(Int) = []
            x
        }""",
        "[]",
        "Vector(Int)"
    )

    test(
        "multi-dimensional vector",
        "[[1, 2, 3], [4, 5, 6], [7, 8, 9]]",
        "[[1, 2, 3], [4, 5, 6], [7, 8, 9]]",
        "Vector(Vector(Int))"
    )

    test(
        "vector of records declaration",
        "[{ a = 65 }, { a = -50 }]",
        "[{ a = 65 }, { a = -50 }]",
        "Vector({ a : Int })"
    )

    test(
        "Boolean vector declaration",
        "[true, false]",
        "[true, false]",
        "Vector(< False : Unit, True : Unit >)"
    )

    test(
        "Boolean vector declaration with operations",
        """[Booleans.and(false, false),
            Booleans.and(false, true),
            Booleans.and(true, false),
            Booleans.and(true, true)]""",
        "[false, false, false, true]",
        "Vector(< False : Unit, True : Unit >)"
    )

    test(
        "Unit vector declaration",
        "[{}]",
        "[{}]",
        "Vector(Unit)"
    )

    test(
        "String vector declaration",
        """["hello", "world"]""",
        """["hello", "world"]""",
        "Vector(String)"
    )

    test(
        "variant Vector declaration",
        "[< a = 1 >]",
        "[< a = 1 >]",
        "Vector(< a : Int >)"
    )

    test(
        "Vector operations - get",
        """{
            val x = [1,2,3]
            Vectors.get(Int, x, 0)
        }""",
        "1",
        "Int"
    )

    test(
        "Vector operations - get - upper bound",
        """{
            val x = [1,2,3]
            Vectors.get(Int, x, 2)
        }""",
        "3",
        "Int"
    )

    test(
        "Vector operations - put on vector",
        """{
            val x : Vector(Int) = [1]
            Vectors.put(Int, x, 0, 5)
        }""",
        "[5]",
        "Vector(Int)"
    )

    test(
        "Vector operations - append",
        """{
            val x = [1,2,3]
            val z = Vectors.append(Int, x, 4)
            z
        }""",
        "[1, 2, 3, 4]",
        "Vector(Int)"
    )

    test(
        "Vector operations - append on empty vector",
        """{
            val x = []
            Vectors.append(Int, x, 4)
        }""",
        "[4]",
        "Vector(Int)"
    )

    test(
        "Vector operations - prepend",
        """{
            val x = [1,2,3]
            val z = Vectors.prepend(Int, x, 4)
            z
        }""",
        "[4, 1, 2, 3]",
        "Vector(Int)"
    )

    test(
        "Vector operations - prepend on empty vector",
        """{
            val x = []
            Vectors.prepend(Int, x, 4)
        }""",
        "[4]",
        "Vector(Int)"
    )

    test(
        "Vector operations - empty vector length",
        """{
            val x = []
            Vectors.length(Int, x)
        }""",
        "0",
        "Int"
    )

    test(
        "Vector operations - vector length",
        """{
            val x = [1,2,3]
            Vectors.length(Int, x)
        }""",
        "3",
        "Int"
    )

    test(
        "Vector operations - empty concat - both",
        """{
            val x = []
            val y = []
            Vectors.concat(Int, x, y)
        }""",
        "[]",
        "Vector(Int)"
    )

    test(
        "Vector operations - empty concat - left",
        """{
            val x = []
            val y = [1]
            Vectors.concat(Int, x, y)
        }""",
        "[1]",
        "Vector(Int)"
    )

    test(
        "Vector operations - empty concat - right",
        """{
            val x = [1]
            val y = []
            Vectors.concat(Int, x, y)
        }""",
        "[1]",
        "Vector(Int)"
    )

    test(
        "Vector operations - concat ",
        """{
            val x = [1,2,3]
            val y = [4,5,6]
            Vectors.concat(Int, x, y)
        }""",
        "[1, 2, 3, 4, 5, 6]",
        "Vector(Int)"
    )

    testError(
        "vector with internal error",
        "[prim IntAdd(1,1), prim IntDiv(1, 0)]",
        "PrimitiveException: IntDiv: division by zero"
    )

    testError(
        "Vector operations - get - out of bounds",
        """{
            val x : Vector(Int) = [1,2,3]
            Vectors.get(Int, x, 4)
        }""",
        "PrimitiveException: VecGet: vector index out of bounds - size: 3, index: 4"
    )

    testError(
        "Vector operations - get - out of bounds - negative index",
        """{
            val x : Vector(Int) = [1,2,3]
            Vectors.get(Int, x, -1)
        }""",
        "PrimitiveException: VecGet: vector index out of bounds - size: 3, index: -1"
    )

    testError(
        "Vector operations - get - out of bounds - empty vector ",
        """{
            val x : Vector(Int) = []
            Vectors.get(Int, x, 0)
        }""",
        "PrimitiveException: VecGet: vector index out of bounds - size: 0, index: 0"
    )

    testError(
        "Vector operations - put on empty vector ",
        """{
            val x : Vector(Int) = []
            Vectors.put(Int, x, 0, 5)
        }""",
        "PrimitiveException: VecPut: vector index out of bounds - size: 0, index: 0"
    )

    testError(
        "Vector operations - put on vector, index out of bounds ",
        """{
            val x : Vector(Int) = [1]
            Vectors.put(Int, x, 1, 5)
        }""",
        "PrimitiveException: VecPut: vector index out of bounds - size: 1, index: 1"
    )

    testError(
        "Vector operations - put on vector, index out of bounds (negative) ",
        """{
            val x : Vector(Int) = [1]
            Vectors.put(Int, x, -1, 5)
        }""",
        "PrimitiveException: VecPut: vector index out of bounds - size: 1, index: -1"
    )

}
