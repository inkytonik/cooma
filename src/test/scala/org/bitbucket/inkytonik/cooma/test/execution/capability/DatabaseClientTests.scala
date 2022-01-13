package org.bitbucket.inkytonik.cooma.test.execution.capability

import java.nio.file.{Files, Paths}

import org.bitbucket.inkytonik.cooma.test.{BackendConfig, ExecutionTests}

class DatabaseClientTests extends ExecutionTests {

    val basePath = "src/test/resources/capability/db"

    def run(filename : String, databases : Int*)(implicit bc : BackendConfig) : String = {
        val now = System.currentTimeMillis()
        val paths = databases.map(i => (s"$basePath/test_$i.db", s"$basePath/test_${i}_$now.db")).toSeq
        paths.foreach { case (base, copy) => Files.copy(Paths.get(base), Paths.get(copy)) }
        val (_, copyPaths) = paths.unzip
        val result = runFile(filename, Seq("-r"), copyPaths)
        copyPaths.foreach(path => Files.delete(Paths.get(path)))
        result
    }

    test("integer columns") { implicit bc =>
        val filename = s"$basePath/integer_columns.cooma"
        val result = runFile(filename, Seq("-r"), Seq(s"$basePath/test_1.db"))
        result shouldBe "[{ id = 1, x = 17, y = 30 }, { id = 2, x = 90, y = 22 }]\n"
    }

    test("string columns") { implicit bc =>
        val filename = s"$basePath/string_columns.cooma"
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

    test(s"subset of columns") { implicit bc =>
        val filename = s"$basePath/subset_of_columns.cooma"
        val result = runFile(filename, Seq("-r"), Seq(s"$basePath/test_1.db"))
        result shouldBe
            """|[{ id = 1, c1 = "hwk", c3 = "diy" }, { id = 2, c1 = "qfd", c3 = "gut" }]
               |""".stripMargin
    }

    test(s"multiple tables") { implicit bc =>
        val filename = s"$basePath/multiple_tables.cooma"
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

    test(s"multiple databases") { implicit bc =>
        val filename = s"$basePath/multiple_databases.cooma"
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

    test(s"non-Table Database field") { implicit bc =>
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
        val filename = s"$basePath/string_columns.cooma"
        val result = runFile(filename, Seq(), Seq(s"$basePath/test_0.db"))
        result shouldBe "PrimitiveException: DatabaseClient: 'src/test/resources/capability/db/test_0.db' is not a file\n"
    }

    test(s"invalid database file") { implicit bc =>
        val filename = s"$basePath/string_columns.cooma"
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

    test("mismatching primary key") { implicit bc =>
        val filename = s"$basePath/mismatching_primary_key.cooma"
        val result = runFile(filename, Seq(), Seq(s"$basePath/test_5.db"))
        result shouldBe
            """|PrimitiveException: DatabaseClient: the following tables have errors:
               |    mismatching_primary_key:
               |        column 'id' is not a primary key
               |        column 'x' is a primary key
               |""".stripMargin
    }

    test("missing primary key") { implicit bc =>
        val filename = s"$basePath/missing_primary_key.cooma"
        val result = runFile(filename, Seq(), Seq(s"$basePath/test_1.db"))
        result shouldBe
            """|PrimitiveException: DatabaseClient: the following tables have errors:
               |    text_columns:
               |        table must have 'id' column
               |""".stripMargin
    }

    test("nullable primary key") { implicit bc =>
        val filename = s"$basePath/nullable_primary_key.cooma"
        val result = runFile(filename, Seq(), Seq(s"$basePath/test_5.db"))
        result shouldBe
            """|PrimitiveException: DatabaseClient: the following tables have errors:
               |    nullable_primary_key:
               |        column 'id' must have type 'Int'
               |""".stripMargin
    }

    test("non-integer primary key") { implicit bc =>
        val filename = s"$basePath/non_integer_primary_key.cooma"
        val result = runFile(filename, Seq(), Seq(s"$basePath/test_5.db"))
        result shouldBe
            """|PrimitiveException: DatabaseClient: the following tables have errors:
               |    non_integer_primary_key:
               |        column 'id' must have type 'Int'
               |""".stripMargin
    }

    test("getById: row exists") { implicit bc =>
        val filename = s"$basePath/get_by_id_row_exists.cooma"
        val result = runFile(filename, Seq("-r"), Seq(s"$basePath/test_1.db"))
        result shouldBe """<< Some = { id = 2, name = "mhu" } >>""" + "\n"
    }

    test("getById: row does not exist") { implicit bc =>
        val filename = s"$basePath/get_by_id_row_does_not_exist.cooma"
        val result = runFile(filename, Seq("-r"), Seq(s"$basePath/test_1.db"))
        result shouldBe "<< None = {} >>\n"
    }

    test("insert: integer") { implicit bc =>
        val filename = s"$basePath/insert_integer.cooma"
        val result = run(filename, 1)
        result shouldBe "<< Some = { id = 3, x = 23, y = 49 } >>\n"
    }

    test("insert: string") { implicit bc =>
        val filename = s"$basePath/insert_string.cooma"
        val result = run(filename, 1)
        result shouldBe """<< Some = { id = 4, name = "abc" } >>""" + "\n"
    }

    test("insert: boolean") { implicit bc =>
        val filename = s"$basePath/insert_boolean.cooma"
        val result = run(filename, 1)
        result shouldBe "<< Some = { id = 2, p = << False = {} >>, q = << True = {} >> } >>\n"
    }

    test("insert: nullable") { implicit bc =>
        val filename = s"$basePath/insert_nullable.cooma"
        val result = run(filename, 1)
        result shouldBe
            """|<< Some = {
               |  id = 5,
               |  name_1 = << Some = "asdf" >>,
               |  name_2 = << None = {} >>
               |} >>
               |""".stripMargin
    }

    test("insert: return integer") { implicit bc =>
        ???
    }

    test("insert: error") { implicit bc =>
        ???
    }

    test("update: integer") { implicit bc =>
        val filename = s"$basePath/update_integer.cooma"
        val result = run(filename, 1)
        result shouldBe "<< Some = { id = 2, x = 3, y = 6 } >>\n"
    }

    test("update: string") { implicit bc =>
        val filename = s"$basePath/update_string.cooma"
        val result = run(filename, 1)
        result shouldBe """<< Some = { id = 2, name = "zxcv" } >>""" + "\n"
    }

    test("update: boolean") { implicit bc =>
        val filename = s"$basePath/update_boolean.cooma"
        val result = run(filename, 1)
        result shouldBe """<< Some = { id = 1, p = << True = {} >>, q = << True = {} >> } >>""" + "\n"
    }

    test("update: nullable") { implicit bc =>
        val filename = s"$basePath/update_nullable.cooma"
        val result = run(filename, 1)
        result shouldBe
            """|<< Some = {
               |  id = 3,
               |  name_1 = << Some = "qwerty" >>,
               |  name_2 = << None = {} >>
               |} >>
               |""".stripMargin
    }

    test("delete") { implicit bc =>
        val filename = s"$basePath/delete.cooma"
        val result = run(filename, 1)
        result shouldBe "<< None = {} >>\n"
    }

    test("escape strings") { implicit bc =>
        ???
    }

}
