package org.bitbucket.inkytonik.cooma.test.semantic

import org.bitbucket.inkytonik.cooma.test.SemanticTests

class BuiltInTests extends SemanticTests {

    test(
        "Boolean is defined",
        "Boolean",
        ""
    )

    test(
        "Boolean is a variant with False and True fields",
        "{fun (b : Boolean) b match { case False(x) => 0 case True(x) => 1}}(true)",
        ""
    )

    test(
        "false is defined",
        "false",
        ""
    )

    test(
        "false is Boolean",
        "{fun (b : Boolean) 0}(false)",
        ""
    )

    test(
        "true is defined",
        "true",
        ""
    )

    test(
        "true is Boolean",
        "{fun (b : Boolean) 0}(true)",
        ""
    )

    test(
        "aliases are used in type error messages",
        "{fun (b : Boolean) 0}(0)",
        """|1:23:error: expected Boolean, got 0 of type Int
           |{fun (b : Boolean) 0}(0)
           |                      ^
           |"""
    )

    test(
        "HttpGet is a record with a get field",
        "fun (httpClient : HttpGet) httpClient.get(\"\")",
        ""
    )

    test(
        "HttpDelete & HttpGet & HttpPost & HttpPut is a record with the respective fields",
        """|fun (httpClient : HttpDelete & HttpGet & HttpPost & HttpPut) {
           |    val _ = httpClient.delete("")
           |    val _ = httpClient.get("")
           |    val _ = httpClient.post("")
           |    val _ = httpClient.put("")
           |    {}
           |}
           |""".stripMargin,
        ""
    )

    test(
        "Reader is a record with a read field",
        "fun (r : Reader) r.read()",
        ""
    )

    test(
        "Reader read field has correct type",
        "{ def f (r : Reader) String = r.read() 0 }",
        ""
    )

    test(
        "Reader doesn't have non-read field",
        "fun (r : Reader) r.foo",
        """|1:20:error: foo is not a field of record type { read : () String }
           |fun (r : Reader) r.foo
           |                   ^
           |"""
    )

    test(
        "Writer is built-in record type",
        """fun (w : Writer) w.write("hello")""",
        ""
    )

    test(
        "Writer write field has correct type",
        """{ def f (w : Writer) Unit = w.write("hi") 0 }""",
        ""
    )

    test(
        "Writer doesn't have non-write field",
        "fun (w : Writer) w.foo",
        """|1:20:error: foo is not a field of record type { write : (s : String) Unit }
           |fun (w : Writer) w.foo
           |                   ^
           |"""
    )

    test(
        "FolderReader has correct type",
        """{ def f (r : FolderReader) String = r.read("a.txt") {} }""",
        ""
    )

    test(
        "FolderWriter has correct type",
        """{ def f (w : FolderWriter) Unit = w.write("a.txt", "text") {} }""",
        ""
    )

}
