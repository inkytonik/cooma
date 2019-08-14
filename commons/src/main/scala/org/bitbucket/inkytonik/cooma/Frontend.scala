package org.bitbucket.inkytonik.cooma

trait Frontend {
    def interpret(config : Config)
    def interpret(programName : String, program : String, config : Config)
    //def repl(env : Env, i : String, printValue : Boolean, config : Config, term : Term) : Env
}
