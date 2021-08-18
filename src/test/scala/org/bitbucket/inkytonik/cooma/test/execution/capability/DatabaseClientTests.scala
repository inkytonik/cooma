package org.bitbucket.inkytonik.cooma.test.execution.capability

import org.bitbucket.inkytonik.cooma.test.ExecutionTests

class DatabaseClientTests extends ExecutionTests {

    val basePath = "src/test/resources/capability"

    val dbPath = s"$basePath/test.db"

    {
        val filename = s"$basePath/dbAll.cooma"
        test(s"run: all ($filename)") { implicit bc =>
            val result = runFile(filename, Seq("-r"), Seq(s"$dbPath:student"))
            result shouldBe
                """|[{
                   |  id = "1",
                   |  name = "John Smith",
                   |  wam = ""
                   |}, {
                   |  id = "2",
                   |  name = "Jane Citizen",
                   |  wam = "87"
                   |}]
                   |""".stripMargin
        }
    }

    {
        val filename = s"$basePath/dbAllColumnSubset.cooma"
        test(s"run: all, subset of columns ($filename)") { implicit bc =>
            val result = runFile(filename, Seq("-r"), Seq(s"$dbPath:student"))
            result shouldBe
                """|[{ name = "John Smith" }, { name = "Jane Citizen" }]
                   |""".stripMargin
        }
    }

    {
        val filename = s"$basePath/dbAllExtraneousColumn.cooma"
        test(s"run: extraneous column ($filename)") { implicit bc =>
            val result = runFile(filename, Seq(), Seq(s"$dbPath:student"))
            result shouldBe "CapabilityException: Table: table student is missing columns: foo, bar\n"
        }
    }

    {
        val filename = s"$basePath/dbNonStringField.cooma"
        test(s"run: non-string field ($filename)") { implicit bc =>
            val result = runFile(filename, Seq(), Seq(s"$dbPath:student"))
            result shouldBe
                """|src/test/resources/capability/dbNonStringField.cooma:1:13:error: illegal main program argument type
                   |fun (table: Table({
                   |            ^
                   |""".stripMargin
        }
    }

    {
        val filename = s"$basePath/dbNonRecordArgument.cooma"
        test(s"run: non-record argument to Table ($filename)") { implicit bc =>
            val result = runFile(filename, Seq(), Seq(s"$dbPath:student"))
            result shouldBe
                """|src/test/resources/capability/dbNonRecordArgument.cooma:1:13:error: illegal main program argument type
                   |fun (table: Table(String)) table.all()
                   |            ^
                   |""".stripMargin
        }
    }

    {
        val filename = s"$basePath/dbAll.cooma"
        test(s"run: non-existent database ($filename)") { implicit bc =>
            val result = runFile(filename, Seq(), Seq(s"$basePath/foo.db:student"))
            result shouldBe "CapabilityException: Table: file src/test/resources/capability/foo.db does not exist\n"
        }
    }

    {
        val filename = s"$basePath/dbAll.cooma"
        test(s"run: invalid database file ($filename)") { implicit bc =>
            val result = runFile(filename, Seq(), Seq(s"$basePath/test0.db:student"))
            result shouldBe "CapabilityException: Table: src/test/resources/capability/test0.db is not an SQLite database\n"
        }
    }

    {
        val filename = s"$basePath/dbAll.cooma"
        test(s"run: non-existent table ($filename)") { implicit bc =>
            val result = runFile(filename, Seq(), Seq(s"$dbPath:foo"))
            result shouldBe "CapabilityException: Table: table foo does not exist\n"
        }
    }

    {
        val filename = s"$basePath/dbAll.cooma"
        test(s"run: invalid characters in table name ($filename)") { implicit bc =>
            val result = runFile(filename, Seq(), Seq(s"$dbPath:*"))
            result shouldBe "CapabilityException: Table: invalid table path 'src/test/resources/capability/test.db:*'\n"
        }
    }

    {
        val filename = s"$basePath/dbAll.cooma"
        test(s"run: missing table name ($filename)") { implicit bc =>
            val result = runFile(filename, Seq(), Seq(s"$dbPath"))
            result shouldBe "CapabilityException: Table: invalid table path 'src/test/resources/capability/test.db'\n"
        }
    }

}
