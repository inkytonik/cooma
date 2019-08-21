package org.bitbucket.inkytonik.cooma

import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
import org.bitbucket.inkytonik.kiama.util.{Console, Source, StringConsole, StringSource}

import scala.collection.mutable.ListBuffer

trait REPLFrontend extends REPL {

    self : Backend =>

    val banner = s"Cooma ${BuildInfo.version} REPL - ${this.backendName} backend \n\nEnter definitions or expressions (:help for commands)"

    override val prompt = "\ncooma> "

    /**
     * Start the REPL
     */
    override def driver(args : Seq[String]) {
        initialise()
        super.driver(args)
    }

    /**
     * Extractor for commands, splits the line into separate words.
     */
    object Command {
        def unapply(line : String) : Option[Seq[String]] = {
            Some((line.trim split ' ').toIndexedSeq)
        }
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
            case line =>
                enterline(config, line)
            case Command(Seq(":quit")) =>
                return None
        }
        Some(config)
    }

    /*
	 * Embed an entry in a program and process it.
	 */
    def processEntry(config: Config, input: REPLInput) = {

        val res = s"res$nResults"
        input match {
            case REPLExpression(e) =>
                nResults = nResults + 1
                processProgram(config, CoomaParserPrettyPrinter.format(
                    Program(Blk(LetVal(Val(res, e), Return(Var(res))))),
                    5).toString, res, printValue = true)

            case REPLDef(fd@Def(i, _, _)) =>
                processProgram(config, CoomaParserPrettyPrinter.format(
                    Program(Blk(LetFun(Vector(fd), Return(Var(i))))),
                    5).toString, res, printValue = false)

            case REPLVal(Val(i, e)) =>
                processProgram(config, CoomaParserPrettyPrinter.format(
                    Program(Blk(LetVal(Val(i, e), Return(Var(i))))),
                    5).toString, res, printValue = true)
        }
    }

    def processProgram(config: Config, line: String, i: String, printValue: Boolean): Unit = {



        currentDynamicEnv = repl(currentDynamicEnv, i, printValue, config, term)

    }

}
