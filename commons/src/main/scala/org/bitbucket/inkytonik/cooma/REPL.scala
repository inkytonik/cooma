package org.bitbucket.inkytonik.cooma

import org.bitbucket.inkytonik.kiama.util.{REPLBase, StringSource}

import scala.collection.mutable.ListBuffer

trait REPL extends REPLBase[Config] {

    self : Compiler with Backend =>

    import org.bitbucket.inkytonik.cooma.BuildInfo
    import org.bitbucket.inkytonik.cooma.CoomaParserPrettyPrinter.{any, layout}
    import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
    import org.bitbucket.inkytonik.kiama.util.{Console, Source, StringConsole}

    override val prompt = "\ncooma> "
    val banner = s"Cooma ${BuildInfo.version} REPL - ${this.backendName} backend \n\nEnter definitions or expressions (:help for commands)"
    /**
     * Counter of expression results.
     */
    var nResults = 0
    /**
     * Runtime environment that keeps track of previously bound values.
     */
    var currentDynamicEnv : Env = emptyEnv

    def createConfig(args : Seq[String]) : Config =
        new Config(args)

    /**
     * Start the REPL
     */
    override def driver(args : Seq[String]) {
        initialise()
        super.driver(args)
    }

    /**
     * Initialise REPL state.
     */
    def initialise() {
        currentDynamicEnv = emptyEnv
        nResults = 0
    }

    def help(config : Config) {
        config.output().emit(
            """
			  |exp                    evaluate exp, print value
			  |val x = exp            add new value definition
			  |def f(x : Int) = exp   add new function definition
			  |:help                  print this message
			  |:lines                 enter multiple separate input lines until :end
			  |:paste                 enter single multi-line input until :end
			  |:quit                  quit the REPL (also Control-D)
			  |""".stripMargin
        )
    }

    def getLines(console : Console) : String = {
        val buf = ListBuffer[String]()
        var line = console.readLine("")
        while (line.trim != ":end") {
            buf.append(line + "\n")
            line = console.readLine("")
        }
        buf.mkString
    }

    /**
     * Process a line of user input.
     */
    override def processline(source : Source, console : Console, config : Config) : Option[Config] = {
        source.content match {
            case Command(Seq(":help")) =>
                help(config)
            case Command(Seq(":lines")) =>
                processconsole(new StringConsole(getLines(console)), "", config)
            case Command(Seq(":paste")) =>
                enterline(config, getLines(console))
            case Command(Seq(":quit")) =>
                return None
            case line =>
                enterline(config, line)
        }
        Some(config)
    }

    def enterline(config : Config, line : String) {
        val source = new StringSource(line)
        val p = new CoomaParser(source, positions)
        val pr = p.pWhitespace(0)
        if (!pr.hasValue) {
            val pr = p.pREPLInput(0)
            if (pr.hasValue)
                processEntry(config, p.value(pr).asInstanceOf[REPLInput])
            else
                config.output().emitln(p.formatParseError(pr.parseError, false))
        }
    }

    /**
     * Embed an entry in a program and process it.
     */
    def processEntry(config : Config, input : REPLInput) =
        input match {

            case REPLExpression(e) =>
                val i = s"res$nResults"
                nResults = nResults + 1
                process(Program(Blk(LetVal(Val(IdnDef(i), e), Return(Var(IdnUse(i)))))), i, true, config)

            case REPLDef(fd @ Def(IdnDef(i), _, _)) =>
                process(
                    Program(Blk(LetFun(Vector(fd), Return(Var(IdnUse(i)))))),
                    i, false, config
                )

            case REPLVal(Val(IdnDef(i), e)) =>
                process(Program(Blk(LetVal(Val(IdnDef(i), e), Return(Var(IdnUse(i)))))), i, true, config)

        }

    /**
     * Process the AST from the user's entered text.
     */
    def process(program : Program, i : String, printValue : Boolean, config : Config) {
        if (config.coomaASTPrint())
            config.output().emitln(layout(any(program), 5))
        val term = compileStandalone(program)

        currentDynamicEnv = repl(currentDynamicEnv, i, printValue, config, term)
    }

    /**
     * Extractor for commands, splits the line into separate words.
     */
    object Command {
        def unapply(line : String) : Option[Seq[String]] = {
            Some((line.trim split ' ').toIndexedSeq)
        }
    }

}
