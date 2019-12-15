package org.bitbucket.inkytonik.cooma

import org.bitbucket.inkytonik.kiama.util.{REPLBase, StringSource}

trait REPL extends REPLBase[Config] {

    self : Compiler with Backend =>

    import org.bitbucket.inkytonik.cooma.CoomaParserPrettyPrinter.{any, layout, show}
    import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
    import org.bitbucket.inkytonik.cooma.SymbolTable._
    import org.bitbucket.inkytonik.kiama.relation.Tree
    import org.bitbucket.inkytonik.kiama.util.Messaging.Messages
    import org.bitbucket.inkytonik.kiama.util.{Console, Source, StringConsole}

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
    var currentStaticEnv : Environment = _

    /**
     * Create a configuration for this REPL session.
     */
    def createConfig(args : Seq[String]) : Config =
        new Config(args)

    /**
     * Start the REPL
     */
    override def driver(args : Seq[String]) : Unit = {
        initialise()
        super.driver(args)
    }

    /**
     * Initialise REPL state.
     */
    def initialise() : Unit = {
        currentStaticEnv = rootenv()
        nResults = 0
    }

    def help(config : Config) : Unit = {
        config.output().emit(
            """
			  |exp                        evaluate exp, print value
			  |val x = exp                add new value definition
			  |def f(x : Int) Int = exp   add new function definition
			  |:help                      print this message
			  |:lines                     enter multiple separate input lines until :end or EOF
			  |:paste                     enter single multi-line input until :end or EOF
			  |:quit                      quit the REPL (also EOF)
			  |""".stripMargin
        )
    }

    def getLines(console : Console) : String = {
        val buf = ListBuffer[String]()
        var line = console.readLine("")
        while (line != null && line.trim != ":end") {
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

    def enterline(line : String, config : Config) : Unit = {
        val source = new StringSource(line)
        val p = new CoomaParser(source, positions)
        val pr = p.pWhitespace(0)
        if (!pr.hasValue) {
            val pr = p.pREPLInput(0)
            if (pr.hasValue) {
                val input = p.value(pr).asInstanceOf[REPLInput]
                checkInput(input, config) match {
                    case Left(messages) =>
                        messaging.report(source, messages, config.output())
                    case Right((input, optTypeValue, aliasedType)) =>
                        processInput(input, optTypeValue, aliasedType, config)
                }
            } else
                config.output().emitln(p.formatParseError(pr.parseError, false))
        }
    }

    def defineVal(i : String, t : Expression, e : Expression) : REPLVal = {
        val v = Val(IdnDef(i), Some(t), e)
        currentStaticEnv = define(enter(currentStaticEnv), i, ValueEntity(v))
        REPLVal(v)
    }

    object AlreadyBoundIdn {
        def unapply(e : Expression) : Boolean =
            e match {
                case BoolT() | False() | Idn(IdnUse(_)) | IntT() | Ints() | ReaderT() |
                    ReaderWriterT() | WriterT() | Strings() | StrT() | True() =>
                    true
                case _ =>
                    false
            }
    }

    def checkInput(
        input : REPLInput,
        config : Config
    ) : Either[Messages, (REPLInput, Option[Expression], Expression)] = {
        if (config.coomaASTPrint())
            config.output().emitln(layout(any(input), 5))
        val tree = new Tree[ASTNode, REPLInput](input)
        val analyser = new SemanticAnalyser(tree, enter(currentStaticEnv))
        (analyser.errors, analyser.replTypeValue(input), analyser.aliasedReplType(input)) match {
            case (Vector(), optTypeValue, Some(aliasedInputType)) =>
                val input2 =
                    input match {
                        case REPLDef(Def(IdnDef(i), Body(as, t, e))) =>
                            defineVal(i, aliasedInputType, Fun(as, e))

                        case REPLExp(AlreadyBoundIdn()) =>
                            input

                        case REPLExp(e) =>
                            val i = s"res$nResults"
                            nResults = nResults + 1
                            defineVal(i, aliasedInputType, e)

                        case REPLVal(Val(IdnDef(i), None, e)) =>
                            defineVal(i, aliasedInputType, e)

                        case REPLVal(Val(IdnDef(i), Some(t), e)) =>
                            defineVal(i, t, e)
                    }
                Right((input2, optTypeValue, aliasedInputType))

            case (Vector(), _, None) =>
                sys.error(s"checkInput: couldn't find aliased REPL type for $input")

            case (messages, _, _) =>
                Left(messages)
        }
    }

    /**
     * Embed an input entry for the REPL.
     */
    def processInput(input : REPLInput, optTypeValue : Option[Expression], aliasedType : Expression, config : Config) =
        input match {
            case REPLDef(fd @ Def(IdnDef(i), _)) =>
                processDef(i, fd, optTypeValue, aliasedType, config)
            case REPLExp(e @ AlreadyBoundIdn()) =>
                processIdn(show(input), e, optTypeValue, aliasedType, config)
            case REPLVal(vd @ Val(IdnDef(i), _, e)) =>
                processVal(i, vd, optTypeValue, aliasedType, config)
            case _ =>
                sys.error(s"$input not supported for the moment")
        }

    /**
     * Process a user-entered value binding.
     */
    def processVal(i : String, vd : Val, optTypeValue : Option[Expression], aliasedType : Expression, config : Config) : Unit = {
        val program = Program(Blk(LetVal(vd, Return(Idn(IdnUse(i))))))
        process(program, i, optTypeValue, aliasedType, config)
    }

    /**
     * Process a user-entered function definition binding.
     */
    def processDef(i : String, fd : Def, optTypeValue : Option[Expression], aliasedType : Expression, config : Config) : Unit = {
        val program = Program(Blk(LetDef(Defs(Vector(fd)), Return(Idn(IdnUse(i))))))
        process(program, i, optTypeValue, aliasedType, config)
    }

    /**
     * Process a user-entered identifier expression.
     */
    def processIdn(i : String, e : Expression, optTypeValue : Option[Expression], aliasedType : Expression, config : Config) : Unit = {
        val program = Program(e)
        process(program, i, optTypeValue, aliasedType, config)
    }

    /**
     * Process the AST from the user's entered text.
     */
    def process(
        program : Program,
        i : String,
        optTypeValue : Option[Expression],
        alaisedType : Expression,
        config : Config
    ) : Unit

    /**
     * Execute a REPL line. If the type of the line is Type, just
     * print that fact, otherwise used eval to do the actual evaluation.
     */
    def execute(
        i : String,
        optTypeValue : Option[Expression],
        aliasedType : Expression,
        config : Config,
        eval : => Unit
    ) =
        if (aliasedType == TypT())
            output(i, optTypeValue, aliasedType, None, config)
        else
            eval

    /**
     * Output in the REPL with name, type and optional result.
     */
    def output(
        i : String,
        optTypeValue : Option[Expression],
        aliasedType : Expression,
        optResult : Option[OutputValueR],
        config : Config
    ) : Unit = {
        val value =
            aliasedType match {
                case TypT() =>
                    optTypeValue match {
                        case Some(typeValue) =>
                            s" = ${show(typeValue)}"
                        case None =>
                            ""
                    }
                case _ =>
                    optResult match {
                        case Some(result) =>
                            s" = ${showRuntimeValue(result)}"
                        case None =>
                            ""
                    }
            }
        config.output().emitln(s"$i : ${show(aliasedType)}$value")
    }

    /**
     * Output in the REPL for error messages
     */
    def errorOutput(optResult : Option[OutputValueR], config : Config) = {
        config.output().emitln(optResult match {
            case Some(result) => showRuntimeValue(result)
            case None         => ""
        })
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
