package org.bitbucket.inkytonik.cooma.test.execution.capability

import java.nio.file.{Files, Paths}

import org.bitbucket.inkytonik.cooma.Util
import org.bitbucket.inkytonik.cooma.test.ExecutionTests
import org.bitbucket.inkytonik.kiama.util.FileSource
import org.bitbucket.inkytonik.kiama.util.Filenames.makeTempFilename
import org.bitbucket.inkytonik.kiama.util.IO.{createFile, deleteFile}

class FileIoTests extends ExecutionTests {

    {
        val filename = "src/test/resources/capability/readerCmdArg.cooma"
        val name = s"reader external argument ($filename)"
        val reader = makeTempFilename(".txt")
        val content = "Contents to be read\n"
        val expectedResult = "\"" + s"${Util.escape(content)}" + "\"" + "\n"
        val args = Seq(reader)

        test(s"run: $name") { implicit bc =>
            createFile(reader, content)
            val result = runFile(filename, Seq(), args)
            result shouldBe ""
            deleteFile(reader)
        }

        test(s"run: $name: result") { implicit bc =>
            createFile(reader, content)
            val result = runFile(filename, Seq("-r"), args)
            result shouldBe expectedResult
            deleteFile(reader)
        }
    }

    {
        val filename = "src/test/resources/capability/writerCmdArg.cooma"
        val name = s"writer command argument ($filename)"
        val writer = makeTempFilename(".txt")
        val args = Seq(writer)
        val content = "Hello world!\n"

        test(s"run: $name") { implicit bc =>
            createFile(writer, "")
            val result = runFile(filename, Seq(), args)
            result shouldBe ""
            FileSource(writer).content shouldBe content
            deleteFile(writer)
        }

        test(s"run: $name: result") { implicit bc =>
            createFile(writer, "")
            val result = runFile(filename, Seq("-r"), args)
            result shouldBe "{}\n"
            FileSource(writer).content shouldBe content
            deleteFile(writer)
        }

        test(s"run: $name: non-existent writer") { implicit bc =>
            val writer = "notThere.txt"
            val result = runFile(filename, Seq(), Seq(writer))
            result shouldBe s"cooma: Writer capability unavailable: can't write $writer\n"
            Files.exists(Paths.get(writer)) shouldBe false
        }

        test(s"run: $name: no args") { implicit bc =>
            val result = runFile(filename, Seq(), Seq())
            result shouldBe s"cooma: command-line argument 0 does not exist (arg count = 0)\n"
        }

        test(s"run: $name: standard out") { implicit bc =>
            val result = runFile(filename, Seq(), Seq("-"))
            result shouldBe content
        }

    }

    {
        val sourceFilename = "src/test/resources/capability/folderReaderCmdArg.cooma"
        val name = s"FolderReader command arguments ($sourceFilename)"
        val root = Paths.get("./src/main/resources/tmp")
        val sub = root.resolve("sub")
        val a = root.resolve("a.txt")
        val b = sub.resolve("b.txt")

        test(s"run: $name") { implicit bc =>
            sub.toFile.mkdirs()
            Files.write(a, "text a".getBytes)
            Files.write(b, "text b".getBytes)
            val result = runFile(sourceFilename, Seq("-r"), Seq(root.toString))
            result shouldBe "\"text atext b\"\n"
            b.toFile.delete()
            a.toFile.delete()
            sub.toFile.delete()
        }
    }

    {
        val sourceFilename = "src/test/resources/capability/folderWriterCmdArg.cooma"
        val name = s"FolderWriter command arguments ($sourceFilename)"
        val root = Paths.get("./src/main/resources/tmp")
        val sub = root.resolve("sub")
        val a = root.resolve("a.txt")
        val b = sub.resolve("b.txt")

        test(s"run: $name") { implicit bc =>
            sub.toFile.mkdirs()
            val result = runFile(sourceFilename, Seq(), Seq(root.toString))
            result shouldBe ""
            new String(Files.readAllBytes(a)) shouldBe "text a"
            new String(Files.readAllBytes(b)) shouldBe "text b"
            b.toFile.delete()
            a.toFile.delete()
            sub.toFile.delete()
        }
    }

    {
        val sourceFilename = "src/test/resources/capability/folderReaderDescendantCheckFail.cooma"
        val name = s"FolderWriter command arguments ($sourceFilename): descendant check fail"
        val root = Paths.get("./src/main/resources/tmp/sub")

        test(s"run: $name") { implicit bc =>
            val result = runFile(sourceFilename, Seq(), Seq(root.toString))
            result shouldBe "cooma: FolderReaderRead ./src/main/resources/tmp/sub: ./src/main/resources/tmp/sub/../a.txt is not a descendant of ./src/main/resources/tmp/sub\n"
        }
    }

    {
        val filename = "src/test/resources/capability/writerAndReaderCmdArg.cooma"
        val name = s"writer and reader command arguments ($filename)"
        val writer = makeTempFilename(".txt")
        val reader = makeTempFilename(".txt")
        val args = Seq(writer, reader)
        val content = "The file contents"

        test(s"run: $name") { implicit bc =>
            createFile(writer, "")
            createFile(reader, content)
            val result = runFile(filename, Seq(), args)
            result shouldBe ""
            FileSource(writer).content shouldBe content
            FileSource(reader).content shouldBe content
            deleteFile(writer)
            deleteFile(reader)
        }

        test(s"run: $name: result") { implicit bc =>
            createFile(writer, "")
            createFile(reader, content)
            val result = runFile(filename, Seq("-r"), args)
            result shouldBe "{}\n"
            FileSource(writer).content shouldBe content
            FileSource(reader).content shouldBe content
            deleteFile(writer)
            deleteFile(reader)
        }

        test(s"run: $name: non-existent writer") { implicit bc =>
            createFile(reader, "")
            val writer = "notThere.txt"
            val result = runFile(filename, Seq(), Seq(writer, reader))
            result shouldBe s"cooma: Writer capability unavailable: can't write $writer\n"
            Files.exists(Paths.get(writer)) shouldBe false
            deleteFile(writer)
        }

        test(s"run: $name: non-existent reader") { implicit bc =>
            createFile(writer, "")
            val reader = "notThere.txt"
            val result = runFile(filename, Seq(), Seq(writer, reader))
            result shouldBe s"cooma: Reader capability unavailable: can't read $reader\n"
            Files.exists(Paths.get(reader)) shouldBe false
            deleteFile(writer)
        }

        test(s"run: $name: no args") { implicit bc =>
            val result = runFile(filename, Seq(), Seq())
            result shouldBe s"cooma: command-line argument 1 does not exist (arg count = 0)\n"
        }

        test(s"run: $name: one arg") { implicit bc =>
            createFile(writer, "")
            val result = runFile(filename, Seq(), Seq(writer))
            result shouldBe s"cooma: command-line argument 1 does not exist (arg count = 1)\n"
            deleteFile(writer)
        }
    }

    {
        val filename = "src/test/resources/capability/readerWriterCmdArg.cooma"
        val name = s"Reader & Writer command arguments ($filename)"
        val rw = makeTempFilename(".txt")
        val args = Seq(rw)

        test(s"run: $name") { implicit bc =>
            createFile(rw, "The file contents\n")
            val result = runFile(filename, Seq(), args)
            result shouldBe ""
            FileSource(rw).content shouldBe "Hello, world!\n"
            deleteFile(rw)
        }

        test(s"run: $name: result") { implicit bc =>
            createFile(rw, "The file contents\n")
            val result = runFile(filename, Seq("-r"), args)
            result shouldBe "\"The file contents\\n\"\n"
            FileSource(rw).content shouldBe "Hello, world!\n"
            deleteFile(rw)
        }
    }

}
