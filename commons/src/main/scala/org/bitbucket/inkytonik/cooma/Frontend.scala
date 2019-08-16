package org.bitbucket.inkytonik.cooma

trait Frontend {
    def interpret(config : Config)
    def interpret(programName : String, program : String, config : Config)
}
