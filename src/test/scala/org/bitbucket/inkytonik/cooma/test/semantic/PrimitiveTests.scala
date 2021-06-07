package org.bitbucket.inkytonik.cooma.test.semantic

import org.bitbucket.inkytonik.cooma.Primitives._
import org.bitbucket.inkytonik.cooma.test.SemanticTests

class PrimitiveTests extends SemanticTests {

    for (op <- allInt1PrimBinOps) {
        val name = primName(op)

        test(
            s"wrong number of arguments for $name primitive (no args)",
            s"prim $name()",
            s"""|1:1:error: primitive $name expects 1 arguments got 0
                |prim $name()
                |^
                |"""
        )

        test(
            s"wrong number of arguments for $name primitive (more)",
            s"prim $name(2,2)",
            s"""|1:1:error: primitive $name expects 1 arguments got 2
                |prim $name(2,2)
                |^
                |"""
        )

        test(
            s"wrong argument type for $name primitive",
            s"""prim $name(\"2\")""",
            s"""|1:13:error: expected Int, got "2" of type String
                |prim $name("2")
                |            ^
                |"""
        )
    }

    for (op <- allInt2PrimBinOps) {
        val name = primName(op)

        test(
            s"wrong number of arguments for $name primitive (no args)",
            s"prim $name()",
            s"""|1:1:error: primitive $name expects 2 arguments got 0
                |prim $name()
                |^
                |"""
        )

        test(
            s"wrong number of arguments for $name primitive (less)",
            s"prim $name(2)",
            s"""|1:1:error: primitive $name expects 2 arguments got 1
                |prim $name(2)
                |^
                |"""
        )

        test(
            s"wrong number of arguments for $name primitive (more)",
            s"prim $name(2,2,2)",
            s"""|1:1:error: primitive $name expects 2 arguments got 3
                |prim $name(2,2,2)
                |^
                |"""
        )

        test(
            s"wrong argument type for $name primitive",
            s"""prim $name(\"2\",2)""",
            s"""|1:13:error: expected Int, got "2" of type String
                |prim $name("2",2)
                |            ^
                |"""
        )

        test(
            s"wrong argument type (cont) for $name primitive",
            s"""prim $name(2,\"2\")""",
            s"""|1:15:error: expected Int, got "2" of type String
                |prim $name(2,"2")
                |              ^
                |"""
        )

        test(
            s"wrong argument type and number of arguments for $name primitive",
            s"""prim $name(2,\"2\",2)""",
            s"""|1:1:error: primitive $name expects 2 arguments got 3
                |prim $name(2,"2",2)
                |^
                |"""
        )
    }

    for (op <- allIntPrimRelOps) {
        val name = primName(op)
        val col = 7 + name.length
        val ind = " " * (col - 1)

        test(
            s"wrong number of arguments for $name primitive (no args)",
            s"prim $name()",
            s"""|1:1:error: primitive $name expects 2 arguments got 0
                |prim $name()
                |^
                |"""
        )

        test(
            s"wrong number of arguments for $name primitive (less)",
            s"prim $name(2)",
            s"""|1:1:error: primitive $name expects 2 arguments got 1
                |prim $name(2)
                |^
                |"""
        )

        test(
            s"wrong number of arguments for $name primitive (more)",
            s"prim $name(2,2,2)",
            s"""|1:1:error: primitive $name expects 2 arguments got 3
                |prim $name(2,2,2)
                |^
                |"""
        )

        test(
            s"wrong argument type for $name primitive",
            s"""prim $name(\"2\",2)""",
            s"""|1:$col:error: expected Int, got "2" of type String
                |prim $name("2",2)
                |$ind^
                |"""
        )

        test(
            s"wrong argument type (cont) for $name primitive",
            s"""prim $name(2,\"2\")""",
            s"""|1:${col + 2}:error: expected Int, got "2" of type String
                |prim $name(2,"2")
                |$ind  ^
                |"""
        )

        test(
            s"wrong argument type and number of arguments for $name primitive",
            s"""prim $name(2,\"2\",2)""",
            s"""|1:1:error: primitive $name expects 2 arguments got 3
                |prim $name(2,"2",2)
                |^
                |"""
        )
    }

    test(
        "wrong number of arguments for StrConcat primitive (no args)",
        "prim StrConcat()",
        """|1:1:error: primitive StrConcat expects 2 arguments got 0
           |prim StrConcat()
           |^
           |"""
    )

    test(
        "wrong number of arguments for StrConcat primitive (two args)",
        """prim StrConcat("hello")""",
        """|1:1:error: primitive StrConcat expects 2 arguments got 1
           |prim StrConcat("hello")
           |^
           |"""
    )

    test(
        "wrong number of arguments for StrConcat primitive (more)",
        """prim StrConcat("hello", "there", "bob")""",
        """|1:1:error: primitive StrConcat expects 2 arguments got 3
           |prim StrConcat("hello", "there", "bob")
           |^
           |"""
    )

    test(
        "wrong argument type for StrConcat primitive",
        """prim StrConcat(2,"2")""",
        """|1:16:error: expected String, got 2 of type Int
           |prim StrConcat(2,"2")
           |               ^
           |"""
    )

    test(
        "wrong argument type for StrConcat primitive (more)",
        """prim StrConcat("2",2)""",
        """|1:20:error: expected String, got 2 of type Int
           |prim StrConcat("2",2)
           |                   ^
           |"""
    )

    test(
        "wrong number of arguments for StrLength primitive (no args)",
        "prim StrLength()",
        """|1:1:error: primitive StrLength expects 1 arguments got 0
           |prim StrLength()
           |^
           |"""
    )

    test(
        "wrong number of arguments for StrLength primitive (two args)",
        """prim StrLength("hello", "there")""",
        """|1:1:error: primitive StrLength expects 1 arguments got 2
           |prim StrLength("hello", "there")
           |^
           |"""
    )

    test(
        "wrong number of arguments for StrLength primitive (more)",
        """prim StrLength("hello", 3, "bob")""",
        """|1:1:error: primitive StrLength expects 1 arguments got 3
           |prim StrLength("hello", 3, "bob")
           |^
           |"""
    )

    test(
        "wrong argument type for StrLength primitive",
        """prim StrLength(2)""",
        """|1:16:error: expected String, got 2 of type Int
           |prim StrLength(2)
           |               ^
           |"""
    )

    test(
        "wrong number of arguments for StrSubstr primitive (no args)",
        "prim StrSubstr()",
        """|1:1:error: primitive StrSubstr expects 2 arguments got 0
           |prim StrSubstr()
           |^
           |"""
    )

    test(
        "wrong number of arguments for StrSubstr primitive (one arg)",
        """prim StrSubstr("hello")""",
        """|1:1:error: primitive StrSubstr expects 2 arguments got 1
           |prim StrSubstr("hello")
           |^
           |"""
    )

    test(
        "wrong number of arguments for StrSubstr primitive (more)",
        """prim StrSubstr("hello", 3, "bob")""",
        """|1:1:error: primitive StrSubstr expects 2 arguments got 3
           |prim StrSubstr("hello", 3, "bob")
           |^
           |"""
    )

    test(
        "wrong argument type for StrSubstr primitive",
        """prim StrSubstr(2,2)""",
        """|1:16:error: expected String, got 2 of type Int
           |prim StrSubstr(2,2)
           |               ^
           |"""
    )

    test(
        "wrong argument type for StrSubstr primitive (more)",
        """prim StrSubstr("2","2")""",
        """|1:20:error: expected Int, got "2" of type String
           |prim StrSubstr("2","2")
           |                   ^
           |"""
    )

    test(
        "wrong argument type for VecAppend primitive (type)",
        "prim VecAppend(1, 1, 1)",
        """|1:16:error: expected Type, got 1 of type Int
           |prim VecAppend(1, 1, 1)
           |               ^
           |"""
    )

    test(
        "wrong argument type for VecConcat primitive (type)",
        """prim VecConcat(1, 1, 1)""",
        """|1:16:error: expected Type, got 1 of type Int
           |prim VecConcat(1, 1, 1)
           |               ^
           |"""
    )

    test(
        "wrong argument type for VecGet primitive (type)",
        """prim VecGet(1, 1, "hi")""",
        """|1:13:error: expected Type, got 1 of type Int
           |prim VecGet(1, 1, "hi")
           |            ^
           |1:19:error: expected Int, got "hi" of type String
           |prim VecGet(1, 1, "hi")
           |                  ^
           |"""
    )

    test(
        "wrong argument type for VecLength primitive (type)",
        """prim VecLength(1, 1)""",
        """|1:16:error: expected Type, got 1 of type Int
           |prim VecLength(1, 1)
           |               ^
           |"""
    )

    test(
        "wrong argument type for VecPrepend primitive (type)",
        """prim VecPrepend(1, 1, 1)""",
        """|1:17:error: expected Type, got 1 of type Int
           |prim VecPrepend(1, 1, 1)
           |                ^
           |"""
    )

    test(
        "wrong argument type for VecPut primitive (type)",
        """prim VecPut(1, 1, "hi", 1)""",
        """|1:13:error: expected Type, got 1 of type Int
           |prim VecPut(1, 1, "hi", 1)
           |            ^
           |1:19:error: expected Int, got "hi" of type String
           |prim VecPut(1, 1, "hi", 1)
           |                  ^
           |"""
    )

    test(
        s"full apply equal",
        s"equal(Int, 1, 2)",
        ""
    )

    test(
        s"partial apply equal (type only)",
        s"equal(Int)",
        ""
    )

    test(
        s"partial apply equal (type and arg)",
        s"equal(Int, 1)",
        ""
    )

    test(
        "wrong argument type for equal (type)",
        """equal(1, 1, 1)""",
        """|1:7:error: expected Type, got 1 of type Int
           |equal(1, 1, 1)
           |      ^
           |1:10:error: expected 1, got 1 of type Int
           |equal(1, 1, 1)
           |         ^
           |1:13:error: expected 1, got 1 of type Int
           |equal(1, 1, 1)
           |            ^
           |"""
    )

    test(
        "wrong argument type for equal (value)",
        """equal(Int, 1, "2")""",
        """|1:15:error: expected Int, got "2" of type String
           |equal(Int, 1, "2")
           |              ^
           |"""
    )

    test(
        s"wrong number of arguments for Equal primitive (no args)",
        s"prim Equal()",
        s"""|1:1:error: primitive Equal expects 3 arguments got 0
            |prim Equal()
            |^
            |"""
    )

    test(
        "wrong number of arguments for Equal primitive (more)",
        "prim Equal(Int)",
        """|1:1:error: primitive Equal expects 3 arguments got 1
           |prim Equal(Int)
           |^
           |"""
    )

    test(
        "wrong number of arguments for Equal primitive (more more)",
        "prim Equal(Int, 2)",
        """|1:1:error: primitive Equal expects 3 arguments got 2
           |prim Equal(Int, 2)
           |^
           |"""
    )

    test(
        "wrong argument type for Equal primitive (type)",
        """prim Equal(1, 1, 1)""",
        """|1:12:error: expected Type, got 1 of type Int
           |prim Equal(1, 1, 1)
           |           ^
           |1:15:error: expected 1, got 1 of type Int
           |prim Equal(1, 1, 1)
           |              ^
           |1:18:error: expected 1, got 1 of type Int
           |prim Equal(1, 1, 1)
           |                 ^
           |"""
    )

    test(
        "wrong argument type for Equal primitive (value)",
        """prim Equal(Int, 1, "2")""",
        """|1:20:error: expected Int, got "2" of type String
           |prim Equal(Int, 1, "2")
           |                   ^
           |"""
    )

    test(
        "non-existent primitive",
        "prim DoesNotExist(1, 2)",
        """|1:6:error: user primitive expected
           |prim DoesNotExist(1, 2)
           |     ^
           |"""
    )

}
