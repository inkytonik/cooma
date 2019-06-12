package org.bitbucket.inkytonik.cooma.graalvm

import org.bitbucket.inkytonik.cooma.{Compiler, Driver}

object GraalVMBackendWrap {

    val wrap = new Compiler with GraalVMBackend

    val driver = new Driver
    //
    //    /**
    //     * Compile a program that will run as a command with
    //     * user-supplied command-line arguments.
    //     */
    //    def myCompileCommand(source : Source) : CoomaTermNode = {
    //
    //        val p = driver.makeast(source, new Config(Seq.empty))
    //
    //        p match {
    //            case Left(program) =>
    //                wrap.compileCommand(program)
    //        }
    //    }

}
