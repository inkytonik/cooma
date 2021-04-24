package org.bitbucket.inkytonik.cooma.test.execution

import org.bitbucket.inkytonik.cooma.test.ExecutionTests

class FileTests extends ExecutionTests {

    val resourcesPath = "src/test/resources"

    for (bc <- backends) {
        filetests(s"${bc.name} file", s"${resourcesPath}/basic", ".cooma", ".out",
            argslist = List(bc.options ++ List("-r")))
        filetests(s"${bc.name} file", s"${resourcesPath}/bad", ".cooma", ".out",
            argslist = List(bc.options))
    }

}
