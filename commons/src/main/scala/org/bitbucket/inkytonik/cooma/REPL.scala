package org.bitbucket.inkytonik.cooma

import org.bitbucket.inkytonik.kiama.util.{REPLBase, StringSource}

trait REPL extends REPLBase[Config] {

    self : Compiler with Backend =>

    import org.bitbucket.inkytonik.cooma.BuildInfo
    import org.bitbucket.inkytonik.cooma.CoomaParserPrettyPrinter.{any, layout}
    import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
    import org.bitbucket.inkytonik.kiama.relation.Tree
    import org.bitbucket.inkytonik.kiama.util.{Console, Source, StringConsole}
    import org.bitbucket.inkytonik.kiama.util.Messaging.Messages
    import org.bitbucket.inkytonik.cooma.SymbolTable._
    import scala.collection.mutable.ListBuffer

    override val prompt = "\ncooma> "

    val banner = s"""Cooma ${BuildInfo.version} REPL - ${this.backendName} backend
                    |
                    |Enter definitions or expressions (:help for commands)""".stripMargin

    /**
     * Counter of expression results.
     */
    var nResults = 0

    /**
     * Compile-time environment that keeps track of previously defined entities.
     */
    var currentStaticEnv : Environment = predef

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
        currentStaticEnv = predef
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
                enterline(getLines(console), config)
            case Command(Seq(":quit")) =>
                return None
            case line =>
                enterline(line, config)
        }
        Some(config)
    }

    def enterline(line : String, config : Config) {
        val source = new StringSource(line)
        val p = new CoomaParser(source, positions)
        val pr = p.pWhitespace(0)
        if (!pr.hasValue) {
            val pr = p.pREPLInput(0)
            if (pr.hasValue) {
                val input = p.value(pr).asInstanceOf[REPLInput]
                checkInput(input, config) match {
                    case (Vector(), input, Some(tipe)) =>
                        processInput(input, tipe, config)
                    case (messages, _, _) =>
                        messaging.report(source, messages, config.output())
                }
            } else
                config.output().emitln(p.formatParseError(pr.parseError, false))
        }
    }

    def checkInput(
        input : REPLInput,
        config : Config
    ) : (Messages, REPLInput, Option[Expression]) = {
        val input2 =
            input match {
                case REPLExpression(e) =>
                    val i = s"res$nResults"
                    nResults = nResults + 1
                    REPLVal(Val(IdnDef(i), e))
                case input =>
                    input
            }
        if (config.coomaASTPrint())
            config.output().emitln(layout(any(input2), 5))
        val tree = new Tree[ASTNode, REPLInput](input2)
        val analyser = new SemanticAnalyser(tree, enter(currentStaticEnv))
        currentStaticEnv = analyser.env(input2)
        (analyser.errors, input2, analyser.replType(input2))
    }

    /**
     * Embed an input entry for the REPL.
     */
    def processInput(input : REPLInput, tipe : Expression, config : Config) =
        input match {
            case REPLDef(fd @ Def(IdnDef(i), _)) =>
                processDef(i, fd, tipe, config)
            case REPLVal(vd @ Val(IdnDef(i), e)) =>
                processVal(i, vd, tipe, config)
            case _ =>
                sys.error(s"$input not supported for the moment")
        }

    /**
     * Construct a program that binds a value.
     */
    def makeVal(i : String, vd : Val) : Program =
        Program(Blk(LetVal(vd, Return(Idn(IdnUse(i))))))

    /**
     * Construct a program that binds a function definition.
     */
    def makeDef(i : String, fd : Def) : Program =
        Program(Blk(LetFun(Vector(fd), Return(Idn(IdnUse(i))))))

    /**
     * Process a user-entered value binding.
     */
    def processVal(i : String, vd : Val, tipe : Expression, config : Config) {
        process(makeVal(i, vd), i, tipe, config)
    }

    /**
     * Process a user-entered function definition binding.
     */
    def processDef(i : String, fd : Def, tipe : Expression, config : Config) {
        process(makeDef(i, fd), i, tipe, config)
    }

    /**
     * Process the AST from the user's entered text.
     */
    def process(
        program : Program,
        i : String,
        tipe : Expression,
        config : Config
    ) {
        val term = compileStandalone(program)
        currentDynamicEnv = repl(currentDynamicEnv, i, tipe, config, term)
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
