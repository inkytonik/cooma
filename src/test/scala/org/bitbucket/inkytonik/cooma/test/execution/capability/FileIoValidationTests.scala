package org.bitbucket.inkytonik.cooma.test.execution.capability

import java.io.File

import org.bitbucket.inkytonik.cooma.test.ExecutionTests

class FileIoValidationTests extends ExecutionTests {

    val root = "./src/test/resources/tmp"

    val srcReader = "./src/test/resources/capability/readerCmdArg.cooma"
    val srcWriter = "./src/test/resources/capability/writerCmdArg.cooma"
    val srcFolderReader = "./src/test/resources/capability/folderReaderCmdArg.cooma"
    val srcFolderWriter = "./src/test/resources/capability/folderWriterCmdArg.cooma"

    def reset() : Unit = {
        def aux(file : File) : Unit = {
            file.setReadable(true)
            file.setWritable(true)
            if (file.isDirectory) file.listFiles.foreach(aux)
            file.delete()
        }
        (new File(root)).listFiles.filter(_.getName != ".keep").foreach(aux)
    }

    test("Reader: ok") { implicit bc =>
        val filename = s"$root/a.txt"
        val file = new File(filename)
        file.createNewFile()
        val result = runFile(srcReader, Seq.empty, Seq(filename))
        reset()
        result shouldBe ""
    }

    test("Reader: does not exist") { implicit bc =>
        val filename = s"$root/a.txt"
        val result = runFile(srcReader, Seq.empty, Seq(filename))
        result shouldBe "CapabilityException: Reader: './src/test/resources/tmp/a.txt' does not exist\n"
    }

    test("Reader: is a directory") { implicit bc =>
        val filename = s"$root/a"
        val dir = new File(filename)
        dir.mkdir()
        val result = runFile(srcReader, Seq.empty, Seq(filename))
        reset()
        result shouldBe "CapabilityException: Reader: './src/test/resources/tmp/a' is a directory\n"
    }

    test("Reader: insufficient permissions") { implicit bc =>
        val filename = s"$root/a.txt"
        val file = new File(filename)
        file.createNewFile()
        file.setReadable(false)
        val result = runFile(srcReader, Seq.empty, Seq(filename))
        reset()
        result shouldBe "CapabilityException: Reader: Cannot read './src/test/resources/tmp/a.txt'\n"
    }

    test("Writer: standard out, ok") { implicit bc =>
        val result = runFile(srcWriter, Seq.empty, Seq("-"))
        reset()
        result shouldBe "Hello world!\n"
    }

    test("Writer: exists, ok") { implicit bc =>
        val filename = s"$root/a.txt"
        val file = new File(filename)
        file.createNewFile()
        val result = runFile(srcWriter, Seq.empty, Seq(filename))
        reset()
        result shouldBe ""
    }

    test("Writer: exists, is a directory") { implicit bc =>
        val filename = s"$root/a"
        val dir = new File(filename)
        dir.mkdir()
        val result = runFile(srcWriter, Seq.empty, Seq(filename))
        reset()
        result shouldBe "CapabilityException: Writer: './src/test/resources/tmp/a' is a directory\n"
    }

    test("Writer: exists, insufficient permissions") { implicit bc =>
        val filename = s"$root/a.txt"
        val file = new File(filename)
        file.createNewFile()
        file.setWritable(false)
        val result = runFile(srcWriter, Seq.empty, Seq(filename))
        reset()
        result shouldBe "CapabilityException: Writer: Cannot write './src/test/resources/tmp/a.txt'\n"
    }

    test("Writer: new, ok") { implicit bc =>
        val filename = s"$root/a.txt"
        val result = runFile(srcWriter, Seq.empty, Seq(filename))
        reset()
        result shouldBe ""
    }

    test("Writer: new, parent does not exist") { implicit bc =>
        val filename = s"$root/0/a.txt"
        val result = runFile(srcWriter, Seq.empty, Seq(filename))
        reset()
        result shouldBe "CapabilityException: Writer: Parent of './src/test/resources/tmp/0/a.txt' does not exist\n"
    }

    test("Writer: new, parent is not a directory") { implicit bc =>
        val filename = s"$root/0/a.txt"
        (new File(s"$root/0")).createNewFile()
        val result = runFile(srcWriter, Seq.empty, Seq(filename))
        reset()
        result shouldBe "CapabilityException: Writer: Parent of './src/test/resources/tmp/0/a.txt' is not a directory\n"
    }

    test("Writer: new, insufficient permissions") { implicit bc =>
        val filename = s"$root/0/a.txt"
        val dir = new File(s"$root/0")
        dir.mkdir()
        dir.setWritable(false)
        val result = runFile(srcWriter, Seq.empty, Seq(filename))
        reset()
        result shouldBe "CapabilityException: Writer: Cannot write './src/test/resources/tmp/0/a.txt'\n"
    }

    test("FolderReader: ok") { implicit bc =>
        val filename = s"$root/0"
        // set up expected files so that the program can run successfully
        val dir = new File(filename)
        val a = new File(s"$filename/a.txt")
        val sub = new File(s"$filename/sub")
        val b = new File(s"$filename/sub/b.txt")
        dir.mkdir()
        a.createNewFile()
        sub.mkdir()
        b.createNewFile()
        val result = runFile(srcFolderReader, Seq.empty, Seq(filename))
        reset()
        result shouldBe ""
    }

    test("FolderReader: does not exist") { implicit bc =>
        val filename = s"$root/0"
        val result = runFile(srcFolderReader, Seq.empty, Seq(filename))
        reset()
        result shouldBe "CapabilityException: FolderReader: './src/test/resources/tmp/0' does not exist\n"
    }

    test("FolderReader: is not a directory") { implicit bc =>
        val filename = s"$root/0"
        (new File(filename)).createNewFile()
        val result = runFile(srcFolderReader, Seq.empty, Seq(filename))
        reset()
        result shouldBe "CapabilityException: FolderReader: './src/test/resources/tmp/0' is not a directory\n"
    }

    test("FolderReader: insufficient permissions") { implicit bc =>
        val filename = s"$root/0"
        val dir = new File(filename)
        dir.mkdir()
        dir.setReadable(false)
        val result = runFile(srcFolderReader, Seq.empty, Seq(filename))
        reset()
        result shouldBe "CapabilityException: FolderReader: Cannot read './src/test/resources/tmp/0'\n"
    }

    test("FolderWriter: ok") { implicit bc =>
        val filename = s"$root/0"
        val dir = new File(filename)
        val sub = new File(s"$filename/sub")
        dir.mkdir()
        sub.mkdir()
        val result = runFile(srcFolderWriter, Seq.empty, Seq(filename))
        reset()
        result shouldBe ""
    }

    test("FolderWriter: does not exist") { implicit bc =>
        val filename = s"$root/0"
        val result = runFile(srcFolderWriter, Seq.empty, Seq(filename))
        reset()
        result shouldBe "CapabilityException: FolderWriter: './src/test/resources/tmp/0' does not exist\n"
    }

    test("FolderWriter: is not a directory") { implicit bc =>
        val filename = s"$root/0"
        (new File(filename)).createNewFile()
        val result = runFile(srcFolderWriter, Seq.empty, Seq(filename))
        reset()
        result shouldBe "CapabilityException: FolderWriter: './src/test/resources/tmp/0' is not a directory\n"
    }

    test("FolderWriter: insufficient permissions") { implicit bc =>
        val filename = s"$root/0"
        val dir = new File(filename)
        dir.mkdir()
        dir.setWritable(false)
        val result = runFile(srcFolderWriter, Seq.empty, Seq(filename))
        reset()
        result shouldBe "CapabilityException: FolderWriter: Cannot write './src/test/resources/tmp/0'\n"
    }

}
