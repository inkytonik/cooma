package org.bitbucket.inkytonik.cooma.test.semantic

import org.bitbucket.inkytonik.cooma.Primitives
import org.bitbucket.inkytonik.cooma.test.SemanticTests

class PrimitiveTests extends SemanticTests {

    for (op <- Primitives.allInt1PrimBinOps) {
        val name = op.primName

        test(
            s"Wrong number of arguments for $name primitive (no args)",
            s"prim $name()",
            s"""|1:1:error: primitive $name expects 1 arguments got 0
                |prim $name()
                |^
                |"""
        )

        test(
            s"Wrong number of arguments for $name primitive (more)",
            s"prim $name(2,2)",
            s"""|1:1:error: primitive $name expects 1 arguments got 2
                |prim $name(2,2)
                |^
                |"""
        )

        test(
            s"Wrong argument type for $name primitive",
            s"""prim $name(\"2\")""",
            s"""|1:13:error: expected Int, got "2" of type String
                |prim $name("2")
                |            ^
                |"""
        )
    }

    for (op <- Primitives.allInt2PrimBinOps) {
        val name = op.primName

        test(
            s"Wrong number of arguments for $name primitive (no args)",
            s"prim $name()",
            s"""|1:1:error: primitive $name expects 2 arguments got 0
                |prim $name()
                |^
                |"""
        )

        test(
            s"Wrong number of arguments for $name primitive (less)",
            s"prim $name(2)",
            s"""|1:1:error: primitive $name expects 2 arguments got 1
                |prim $name(2)
                |^
                |"""
        )

        test(
            s"Wrong number of arguments for $name primitive (more)",
            s"prim $name(2,2,2)",
            s"""|1:1:error: primitive $name expects 2 arguments got 3
                |prim $name(2,2,2)
                |^
                |"""
        )

        test(
            s"Wrong argument type for $name primitive",
            s"""prim $name(\"2\",2)""",
            s"""|1:13:error: expected Int, got "2" of type String
                |prim $name("2",2)
                |            ^
                |"""
        )

        test(
            s"Wrong argument type (cont) for $name primitive",
            s"""prim $name(2,\"2\")""",
            s"""|1:15:error: expected Int, got "2" of type String
                |prim $name(2,"2")
                |              ^
                |"""
        )

        test(
            s"Wrong argument type and number of arguments for $name primitive",
            s"""prim $name(2,\"2\",2)""",
            s"""|1:1:error: primitive $name expects 2 arguments got 3
                |prim $name(2,"2",2)
                |^
                |1:15:error: expected Int, got "2" of type String
                |prim $name(2,"2",2)
                |              ^
                |"""
        )
    }

    for (op <- Primitives.allIntPrimRelOps) {
        val name = op.primName
        val col = 7 + name.length
        val ind = " " * (col - 1)

        test(
            s"Wrong number of arguments for $name primitive (no args)",
            s"prim $name()",
            s"""|1:1:error: primitive $name expects 2 arguments got 0
                |prim $name()
                |^
                |"""
        )

        test(
            s"Wrong number of arguments for $name primitive (less)",
            s"prim $name(2)",
            s"""|1:1:error: primitive $name expects 2 arguments got 1
                |prim $name(2)
                |^
                |"""
        )

        test(
            s"Wrong number of arguments for $name primitive (more)",
            s"prim $name(2,2,2)",
            s"""|1:1:error: primitive $name expects 2 arguments got 3
                |prim $name(2,2,2)
                |^
                |"""
        )

        test(
            s"Wrong argument type for $name primitive",
            s"""prim $name(\"2\",2)""",
            s"""|1:$col:error: expected Int, got "2" of type String
                |prim $name("2",2)
                |$ind^
                |"""
        )

        test(
            s"Wrong argument type (cont) for $name primitive",
            s"""prim $name(2,\"2\")""",
            s"""|1:${col + 2}:error: expected Int, got "2" of type String
                |prim $name(2,"2")
                |$ind  ^
                |"""
        )

        test(
            s"Wrong argument type and number of arguments for $name primitive",
            s"""prim $name(2,\"2\",2)""",
            s"""|1:1:error: primitive $name expects 2 arguments got 3
                |prim $name(2,"2",2)
                |^
                |1:${col + 2}:error: expected Int, got "2" of type String
                |prim $name(2,"2",2)
                |$ind  ^
                |"""
        )
    }

    test(
        "Wrong number of arguments for StrConcat primitive (no args)",
        "prim StrConcat()",
        """|1:1:error: primitive StrConcat expects 2 arguments got 0
           |prim StrConcat()
           |^
           |"""
    )

    test(
        "Wrong number of arguments for StrConcat primitive (two args)",
        """prim StrConcat("hello")""",
        """|1:1:error: primitive StrConcat expects 2 arguments got 1
           |prim StrConcat("hello")
           |^
           |"""
    )

    test(
        "Wrong number of arguments for StrConcat primitive (more)",
        """prim StrConcat("hello", "there", "bob")""",
        """|1:1:error: primitive StrConcat expects 2 arguments got 3
           |prim StrConcat("hello", "there", "bob")
           |^
           |"""
    )

    test(
        "Wrong argument type for StrConcat primitive",
        """prim StrConcat(2,"2")""",
        """|1:16:error: expected String, got 2 of type Int
           |prim StrConcat(2,"2")
           |               ^
           |"""
    )

    test(
        "Wrong argument type for StrConcat primitive (more)",
        """prim StrConcat("2",2)""",
        """|1:20:error: expected String, got 2 of type Int
           |prim StrConcat("2",2)
           |                   ^
           |"""
    )

    test(
        "Wrong number of arguments for StrLength primitive (no args)",
        "prim StrLength()",
        """|1:1:error: primitive StrLength expects 1 arguments got 0
           |prim StrLength()
           |^
           |"""
    )

    test(
        "Wrong number of arguments for StrLength primitive (two args)",
        """prim StrLength("hello", "there")""",
        """|1:1:error: primitive StrLength expects 1 arguments got 2
           |prim StrLength("hello", "there")
           |^
           |"""
    )

    test(
        "Wrong number of arguments for StrLength primitive (more)",
        """prim StrLength("hello", 3, "bob")""",
        """|1:1:error: primitive StrLength expects 1 arguments got 3
           |prim StrLength("hello", 3, "bob")
           |^
           |"""
    )

    test(
        "Wrong argument type for StrLength primitive",
        """prim StrLength(2)""",
        """|1:16:error: expected String, got 2 of type Int
           |prim StrLength(2)
           |               ^
           |"""
    )

    test(
        "Wrong number of arguments for StrSubstr primitive (no args)",
        "prim StrSubstr()",
        """|1:1:error: primitive StrSubstr expects 2 arguments got 0
           |prim StrSubstr()
           |^
           |"""
    )

    test(
        "Wrong number of arguments for StrSubstr primitive (one arg)",
        """prim StrSubstr("hello")""",
        """|1:1:error: primitive StrSubstr expects 2 arguments got 1
           |prim StrSubstr("hello")
           |^
           |"""
    )

    test(
        "Wrong number of arguments for StrSubstr primitive (more)",
        """prim StrSubstr("hello", 3, "bob")""",
        """|1:1:error: primitive StrSubstr expects 2 arguments got 3
           |prim StrSubstr("hello", 3, "bob")
           |^
           |"""
    )

    test(
        "Wrong argument type for StrSubstr primitive",
        """prim StrSubstr(2,2)""",
        """|1:16:error: expected String, got 2 of type Int
           |prim StrSubstr(2,2)
           |               ^
           |"""
    )

    test(
        "Wrong argument type for StrSubstr primitive (more)",
        """prim StrSubstr("2","2")""",
        """|1:20:error: expected Int, got "2" of type String
           |prim StrSubstr("2","2")
           |                   ^
           |"""
    )

    test(
        s"Partial apply equal (type only)",
        s"equal(Int)",
        ""
    )

    test(
        s"Partial apply equal (type and arg)",
        s"equal(Int, 1)",
        ""
    )

    test(
        s"Wrong number of arguments for Equal primitive (no args)",
        s"prim Equal()",
        s"""|1:1:error: primitive Equal expects 3 arguments got 0
            |prim Equal()
            |^
            |"""
    )

    test(
        s"Wrong number of arguments for Equal primitive (more)",
        s"prim Equal(Int)",
        s"""|1:1:error: primitive Equal expects 3 arguments got 1
            |prim Equal(Int)
            |^
            |"""
    )

    test(
        s"Wrong number of arguments for Equal primitive (more more)",
        s"prim Equal(Int, 2)",
        s"""|1:1:error: primitive Equal expects 3 arguments got 2
            |prim Equal(Int, 2)
            |^
            |"""
    )

    test(
        s"Wrong argument type for Equal primitive (type)",
        s"""prim Equal(1, 1, 1)""",
        s"""|1:12:error: expected Type, got 1 of type Int
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
        s"Wrong argument type for Equal primitive (value)",
        s"""prim Equal(Int, 1, "2")""",
        s"""|1:20:error: expected Int, got "2" of type String
            |prim Equal(Int, 1, "2")
            |                   ^
            |"""
    )

    test(
        "non-existent primitive",
        "prim DoesNotExist(1, 2)",
        """|1:1:error: primitive DoesNotExist not found
           |prim DoesNotExist(1, 2)
           |^
           |"""
    )

}
