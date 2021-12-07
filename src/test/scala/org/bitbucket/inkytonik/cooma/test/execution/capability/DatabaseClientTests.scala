package org.bitbucket.inkytonik.cooma.test.execution.capability

import org.bitbucket.inkytonik.cooma.test.ExecutionTests

class DatabaseClientTests extends ExecutionTests {

    val basePath = "src/test/resources/capability/db"

    test(s"one database, one table") { implicit bc =>
        val filename = s"$basePath/one_database_one_table.cooma"
        val result = runFile(filename, Seq("-r"), Seq(s"$basePath/test_1.db"))
        result shouldBe
            """|[{
               |  id = 1,
               |  name = "jlr"
               |}, {
               |  id = 2,
               |  name = "mhu"
               |}, {
               |  id = 3,
               |  name = "efl"
               |}]
               |""".stripMargin
    }

    test(s"subset of columns") { implicit bc =>
        val filename = s"$basePath/subset_of_columns.cooma"
        val result = runFile(filename, Seq("-r"), Seq(s"$basePath/test_1.db"))
        result shouldBe
            """|[{ id = 1, c1 = "hwk", c3 = "diy" }, { id = 2, c1 = "qfd", c3 = "gut" }]
               |""".stripMargin
    }

    test(s"one database, multiple tables") { implicit bc =>
        val filename = s"$basePath/one_database_multiple_tables.cooma"
        val result = runFile(filename, Seq("-r"), Seq(s"$basePath/test_1.db"))
        result shouldBe
            """|{
               |  tc = [{
               |    id = 1,
               |    name = "jlr"
               |  }, {
               |    id = 2,
               |    name = "mhu"
               |  }, {
               |    id = 3,
               |    name = "efl"
               |  }],
               |  mtc = [{
               |    id = 1,
               |    c1 = "hwk",
               |    c3 = "diy"
               |  }, {
               |    id = 2,
               |    c1 = "qfd",
               |    c3 = "gut"
               |  }],
               |  ic = [{
               |    id = 1,
               |    x = 17,
               |    y = 30
               |  }, {
               |    id = 2,
               |    x = 90,
               |    y = 22
               |  }]
               |}
               |""".stripMargin
    }

    test(s"multiple databases, multiple tables") { implicit bc =>
        val filename = s"$basePath/multiple_databases_multiple_tables.cooma"
        val args = (1 to 3).map(n => s"$basePath/test_$n.db")
        val result = runFile(filename, Seq("-r"), args)
        result shouldBe
            """|{
               |  tc = [{
               |    id = 1,
               |    name = "jlr"
               |  }, {
               |    id = 2,
               |    name = "mhu"
               |  }, {
               |    id = 3,
               |    name = "efl"
               |  }],
               |  mtc = [{
               |    id = 1,
               |    c1 = "hwk",
               |    c3 = "diy"
               |  }, {
               |    id = 2,
               |    c1 = "qfd",
               |    c3 = "gut"
               |  }],
               |  ic = [{
               |    id = 1,
               |    x = 17,
               |    y = 30
               |  }, {
               |    id = 2,
               |    x = 90,
               |    y = 22
               |  }],
               |  t1 = [{
               |    id = 1,
               |    name = "nbk"
               |  }, {
               |    id = 2,
               |    name = "hnn"
               |  }],
               |  t2 = [{
               |    id = 1,
               |    x = 480,
               |    y = 847
               |  }, {
               |    id = 2,
               |    x = 440,
               |    y = 197
               |  }],
               |  t = [{
               |    id = 1,
               |    x = "nbf"
               |  }]
               |}
               |""".stripMargin
    }

    test(s"non-record Database argument") { implicit bc =>
        val filename = s"$basePath/non_record_database_argument.cooma"
        val result = runFile(filename, Seq(), Seq(s"$basePath/test_1.db"))
        result shouldBe
            """|src/test/resources/capability/db/non_record_database_argument.cooma:1:11:error: illegal main program argument type
               |fun (db : Database(Int)) {}
               |          ^
               |""".stripMargin
    }

    test(s"non-Table database field") { implicit bc =>
        val filename = s"$basePath/non_table_database_field.cooma"
        val result = runFile(filename, Seq(), Seq(s"$basePath/test_1.db"))
        result shouldBe
            """|src/test/resources/capability/db/non_table_database_field.cooma:1:11:error: illegal main program argument type
               |fun (db : Database({
               |          ^
               |""".stripMargin
    }

    test(s"non-record Table argument") { implicit bc =>
        val filename = s"$basePath/non_record_table_argument.cooma"
        val result = runFile(filename, Seq(), Seq(s"$basePath/test_1.db"))
        result shouldBe
            """|src/test/resources/capability/db/non_record_table_argument.cooma:1:11:error: illegal main program argument type
               |fun (db : Database({
               |          ^
               |""".stripMargin
    }

    test("invalid column type") { implicit bc =>
        val filename = s"$basePath/invalid_column_type.cooma"
        val result = runFile(filename, Seq(), Seq(s"$basePath/test_1.db"))
        result shouldBe
            """|src/test/resources/capability/db/invalid_column_type.cooma:1:11:error: illegal main program argument type
               |fun (db : Database({
               |          ^
               |""".stripMargin
    }

    test(s"extraneous column") { implicit bc =>
        val filename = s"$basePath/extraneous_column.cooma"
        val result = runFile(filename, Seq(), Seq(s"$basePath/test_1.db"))
        result shouldBe
            """|PrimitiveException: DatabaseClient: the following tables have errors:
               |    many_text_columns:
               |        column 'e' does not exist
               |""".stripMargin
    }

    test(s"non-existent database file") { implicit bc =>
        val filename = s"$basePath/one_database_one_table.cooma"
        val result = runFile(filename, Seq(), Seq(s"$basePath/test_5.db"))
        result shouldBe "PrimitiveException: DatabaseClient: 'src/test/resources/capability/db/test_5.db' is not a file\n"
    }

    test(s"invalid database file") { implicit bc =>
        val filename = s"$basePath/one_database_one_table.cooma"
        val result = runFile(filename, Seq(), Seq(s"$basePath/test_4.db"))
        result shouldBe "PrimitiveException: DatabaseClient: 'src/test/resources/capability/db/test_4.db' is not an SQLite database\n"
    }

    test(s"non-existent table") { implicit bc =>
        val filename = s"$basePath/non_existent_table.cooma"
        val result = runFile(filename, Seq(), Seq(s"$basePath/test_1.db"))
        result shouldBe
            """|PrimitiveException: DatabaseClient: the following tables have errors:
               |    e:
               |        the table does not exist
               |""".stripMargin
    }

    test(s"boolean columns") { implicit bc =>
        val filename = s"$basePath/boolean_columns.cooma"
        val result = runFile(filename, Seq("-r"), Seq(s"$basePath/test_1.db"))
        result shouldBe "[{ id = 1, p = << True = {} >>, q = << False = {} >> }]\n"
    }

    test("nullable columns") { implicit bc =>
        val filename = s"$basePath/nullable_columns.cooma"
        val result = runFile(filename, Seq("-r"), Seq(s"$basePath/test_1.db"))
        result shouldBe
            """|[{
               |  id = 1,
               |  name_1 = << Some = "iit" >>,
               |  name_2 = << Some = "ics" >>
               |}, {
               |  id = 2,
               |  name_1 = << Some = "cxa" >>,
               |  name_2 = << None = {} >>
               |}, {
               |  id = 3,
               |  name_1 = << None = {} >>,
               |  name_2 = << Some = "pdg" >>
               |}, {
               |  id = 4,
               |  name_1 = << None = {} >>,
               |  name_2 = << None = {} >>
               |}]
               |""".stripMargin
    }

    test("mismatching column types") { implicit bc =>
        val filename = s"$basePath/mismatching_column_types.cooma"
        val result = runFile(filename, Seq(), Seq(s"$basePath/test_1.db"))
        result shouldBe
            """|PrimitiveException: DatabaseClient: the following tables have errors:
               |    mixed_columns:
               |        column 'a' has type 'TEXT', incompatible with Boolean
               |        column 'b' has type 'TEXT', incompatible with Int
               |        column 'b' is nullable
               |""".stripMargin
    }

    test("mismatching nullability") { implicit bc =>
        val filename = s"$basePath/mismatching_nullability.cooma"
        val result = runFile(filename, Seq(), Seq(s"$basePath/test_1.db"))
        result shouldBe
            """|PrimitiveException: DatabaseClient: the following tables have errors:
               |    mixed_columns:
               |        column 'a' is non-nullable
               |        column 'b' is nullable
               |""".stripMargin
    }

}
