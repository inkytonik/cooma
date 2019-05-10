package org.bitbucket.inkytonik.cooma

class Config(args : Seq[String]) extends org.bitbucket.inkytonik.kiama.util.Config(args) {

    lazy val coomaASTPrint = opt[Boolean]("coomaASTPrint", short = 'C',
        descr = "Print the AST of the Cooma program (default: false)",
        default = Some(false))

    lazy val irPrint = opt[Boolean]("irPrint", short = 'i',
        descr = "Print the intermediate representation (default: false)",
        default = Some(false))

    lazy val irASTPrint = opt[Boolean]("irASTPrint", short = 'I',
        descr = "Print the AST of the intermediate representation (default: false)",
        default = Some(false))

    lazy val resultPrint = opt[Boolean]("resultPrint", short = 'r',
        descr = "Print the result value of source program (default: false)",
        default = Some(false))

}
