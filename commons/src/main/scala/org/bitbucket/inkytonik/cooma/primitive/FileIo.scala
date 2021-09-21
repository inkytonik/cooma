package org.bitbucket.inkytonik.cooma.primitive

import org.bitbucket.inkytonik.cooma.Backend

trait FileIo {

    self : Backend =>

    import java.io.File

    import org.bitbucket.inkytonik.cooma.CoomaException.errCap

    def checkReader(path : String) : Unit = {
        val file = new File(path)
        val cap = "Reader"
        if (!file.exists) errCap(cap, s"'$path' does not exist")
        if (file.isDirectory) errCap(cap, s"'$path' is a directory")
        if (!file.canRead) errCap(cap, s"Cannot read '$path'")
    }

    def checkWriter(path : String) : Unit = {
        if (path != "-") {
            val file = new File(path)
            val cap = "Writer"
            if (file.exists) {
                if (file.isDirectory) errCap(cap, s"'$path' is a directory")
                if (!file.canWrite) errCap(cap, s"Cannot write '$path'")
            } else {
                val parent = file.getParentFile
                if (!parent.exists) errCap(cap, s"Parent of '$path' does not exist")
                if (!parent.isDirectory) errCap(cap, s"Parent of '$path' is not a directory")
                if (!parent.canWrite) errCap(cap, s"Cannot write '$path'")
            }
        }
    }

    def checkFolderReader(path : String) : Unit = {
        val dir = new File(path)
        val cap = "FolderReader"
        if (!dir.exists) errCap(cap, s"'$path' does not exist")
        if (!dir.isDirectory) errCap(cap, s"'$path' is not a directory")
        if (!dir.canRead) errCap(cap, s"Cannot read '$path'")
    }

    def checkFolderWriter(path : String) : Unit = {
        val dir = new File(path)
        val cap = "FolderWriter"
        if (!dir.exists) errCap(cap, s"'$path' does not exist")
        if (!dir.isDirectory) errCap(cap, s"'$path' is not a directory")
        if (!dir.canWrite) errCap(cap, s"Cannot write '$path'")
    }

}
