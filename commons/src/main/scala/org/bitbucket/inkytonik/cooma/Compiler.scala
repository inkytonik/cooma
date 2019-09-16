package org.bitbucket.inkytonik.cooma

trait Compiler {

    self : Backend =>

    import org.bitbucket.inkytonik.cooma.CoomaParserPrettyPrinter.show
    import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
    import org.bitbucket.inkytonik.cooma.SymbolTable.{unit, unitType}
    import org.bitbucket.inkytonik.cooma.Util.{fresh, resetFresh, unescape}

    /**
     * Compile a program that will run as a command with
     * user-supplied command-line arguments.
     */
    def compileCommand(prog : Program) : Term = {
        resetFresh()
        compileTop(prog.expression, 0)
    }

    /**
     * Compile a program that is evaluated as an expression
     * with no command-line arguments.
     */
    def compileStandalone(prog : Program) : Term = {
        resetFresh()
        compileHalt(prog.expression)
    }

    /**
     * Name of capability type?
     */
    def isCapabilityName(n : String) : Boolean =
        (n == "Writer") || (n == "Reader") || (n == "ReaderWriter")

    /**
     * Compile an expression to produce its value via the halt
     * continuation.
     */
    def compileHalt(exp : Expression) : Term =
        compile(exp, z => appC("$halt", z))

    def compileTop(exp : Expression, nArg : Int) : Term = {

        def compileTopArg(a : String, t : Type, e : Expression) : Term =
            t match {
                case IdnT(IdnUse(n)) if isCapabilityName(n) =>
                    val x = fresh("x")
                    letV(x, prmV(argumentP(nArg), Vector()),
                        letV(a, prmV(capabilityP(n), Vector(x)),
                            compileTop(e, nArg + 1)))

                case StrT() =>
                    letV(a, prmV(argumentP(nArg), Vector()),
                        compileTop(e, nArg + 1))

                case _ =>
                    sys.error(s"compileTopArg: ${show(t)} arguments not supported")
            }

        exp match {
            case Fun(Arguments(Vector(Argument(IdnDef(a), t))), e) =>
                compileTopArg(a, t, e)
            case Fun(Arguments(Argument(IdnDef(a), t) +: as), e) =>
                compileTopArg(a, t, Fun(Arguments(as), e))
            case _ =>
                compileHalt(exp)
        }
    }

    def compile(exp : Expression, kappa : String => Term) : Term =
        exp match {
            case App(f, Vector()) =>
                compile(App(f, Vector(unit)), kappa)

            case App(f, Vector(e)) =>
                val k = fresh("k")
                val x = fresh("x")
                compile(f, y =>
                    compile(e, z =>
                        letC(k, x, kappa(x),
                            appF(y, k, z))))

            case App(f, e +: es) =>
                compile(App(App(f, Vector(e)), es), kappa)

            case Blk(be) =>
                compileBlockExp(be, kappa)

            case Cat(l, r) =>
                val x = fresh("x")
                compile(l, y =>
                    compile(r, z =>
                        letV(x, prmV(rowConcatP(), Vector(y, z)),
                            kappa(x))))

            case Fun(Arguments(Vector()), e) =>
                compileFun("_", unitType, e, kappa)

            case Fun(Arguments(Vector(Argument(IdnDef(x), t))), e) =>
                compileFun(x, t, e, kappa)

            case Fun(Arguments(Argument(IdnDef(x), t) +: as), e) =>
                compileFun(x, t, Fun(Arguments(as), e), kappa)

            case Var(IdnUse(i)) =>
                kappa(i)

            case Num(i) =>
                val x = fresh("x")
                letV(x, intV(i),
                    kappa(x))

            case Rec(fields) =>
                val x = fresh("x")
                compileRow(fields, fvs => letV(x, rowV(fvs), kappa(x)))

            case Sel(r, FieldUse(f)) =>
                val x = fresh("x")
                compile(r, z =>
                    letV(x, prmV(rowSelectP(), Vector(z, f)),
                        kappa(x)))

            case Str(s) =>
                val x = fresh("x")
                letV(x, strV(unescape(s.tail.init)),
                    kappa(x))

        }

    def compileCapFun(n : String, x : String, e : Expression, kappa : String => Term) : Term = {
        val f = fresh("f")
        val j = fresh("j")
        val y = fresh("y")
        letV(f, funV(j, y,
            letV(x, prmV(capabilityP(n), Vector(y)),
                tailCompile(e, j))),
            kappa(f))
    }

    def compileFun(x : String, t : Type, e : Expression, kappa : String => Term) : Term =
        t match {
            case IdnT(IdnUse(n)) if isCapabilityName(n) =>
                compileCapFun(n, x, e, kappa)

            case _ =>
                val f = fresh("f")
                val k = fresh("k")
                letV(f, funV(k, x, tailCompile(e, k)),
                    kappa(f))
        }

    def compileBlockExp(be : BlockExp, kappa : String => Term) : Term =
        be match {
            case LetFun(ds, be2) =>
                letF(
                    ds.map(compileDef),
                    compileBlockExp(be2, kappa)
                )

            case LetVal(Val(IdnDef(x), e), be2) =>
                val j = fresh("j")
                letC(j, x, compileBlockExp(be2, kappa),
                    tailCompile(e, j))

            case LetTyp(_, be2) =>
                compileBlockExp(be2, kappa)

            case Return(e) =>
                compile(e, kappa)
        }

    def compileDefBody(args : Vector[Argument], e : Expression, k : String) : Term =
        if (args.isEmpty)
            tailCompile(e, k)
        else
            tailCompile(Fun(Arguments(args), e), k)

    def compileDef(fd : Def) : DefTerm = {
        val k = fresh("k")
        fd match {
            case Def(IdnDef(f), Body(Arguments(Vector()), t, e)) =>
                compileDef(Def(IdnDef(f), Body(Arguments(Vector(Argument(IdnDef("_"), unitType))), t, e)))

            case Def(IdnDef(f), Body(Arguments(Argument(IdnDef(x), IdnT(IdnUse(n))) +: otherArgs), _, e)) if isCapabilityName(n) =>
                val y = fresh("y")
                defTerm(f, k, y, letV(x, prmV(capabilityP(n), Vector(y)),
                    compileDefBody(otherArgs, e, k)))

            case Def(IdnDef(f), Body(Arguments(Argument(IdnDef(x), _) +: otherArgs), _, e)) =>
                defTerm(f, k, x, compileDefBody(otherArgs, e, k))
        }
    }

    def compileRow(
        fields : Vector[Field],
        kappa : Vector[FieldValue] => Term
    ) : Term =
        fields match {
            case Field(f, e) +: t =>
                compile(e, z =>
                    compileRow(t, fvs => kappa(fieldValue(f, z) +: fvs)))

            case Vector() =>
                kappa(Vector())
        }

    def tailCompile(exp : Expression, k : String) : Term =
        exp match {
            case App(f, Vector()) =>
                tailCompile(App(f, Vector(unit)), k)

            case App(e, Vector(a)) =>
                compile(e, x1 =>
                    compile(a, x2 =>
                        appF(x1, k, x2)))

            case App(e, a +: as) =>
                tailCompile(App(App(e, Vector(a)), as), k)

            case Blk(be) =>
                tailCompileBlockExp(be, k)

            case Cat(l, r) =>
                val x = fresh("x")
                compile(l, y =>
                    compile(r, z =>
                        letV(x, prmV(rowConcatP(), Vector(y, z)),
                            appC(k, x))))

            case Fun(Arguments(Vector()), e) =>
                tailCompileFun("_", unitType, e, k)

            case Fun(Arguments(Vector(Argument(IdnDef(x), t))), e) =>
                tailCompileFun(x, t, e, k)

            case Fun(Arguments(Argument(IdnDef(x), t) +: as), e) =>
                tailCompileFun(x, t, Fun(Arguments(as), e), k)

            case Fun(Arguments(a +: as), e) =>
                tailCompile(Fun(Arguments(Vector(a)), Fun(Arguments(as), e)), k)

            case Var(IdnUse(x)) =>
                appC(k, x)

            case Num(i) =>
                val x = fresh("x")
                letV(x, intV(i),
                    appC(k, x))

            case Rec(fields) =>
                val x = fresh("x")
                compileRow(fields, fvs => letV(x, rowV(fvs), appC(k, x)))

            case Sel(e, FieldUse(f)) =>
                val x = fresh("x")
                compile(e, z =>
                    letV(x, prmV(rowSelectP(), Vector(z, f)),
                        appC(k, x)))

            case Str(s) =>
                val x = fresh("x")
                letV(x, strV(unescape(s.tail.init)),
                    appC(k, x))
        }

    def tailCompileCapFun(n : String, x : String, e : Expression, k : String) : Term = {
        val f = fresh("f")
        val j = fresh("j")
        val y = fresh("y")
        letV(f, funV(j, y,
            letV(x, prmV(capabilityP(n), Vector(y)),
                tailCompile(e, j))),
            appC(k, x))
    }

    def tailCompileFun(x : String, t : Type, e : Expression, k : String) : Term =
        t match {
            case IdnT(IdnUse(n)) if isCapabilityName(n) =>
                tailCompileCapFun(n, x, e, k)

            case _ =>
                val f = fresh("f")
                val j = fresh("j")
                letV(f, funV(j, x, tailCompile(e, j)),
                    appC(k, f))
        }

    def tailCompileBlockExp(be : BlockExp, k : String) : Term =
        be match {
            case LetFun(ds, be2) =>
                letF(
                    ds.map(compileDef),
                    tailCompileBlockExp(be2, k)
                )

            case LetVal(Val(IdnDef(x), e), be2) =>
                val j = fresh("j")
                letC(j, x, tailCompileBlockExp(be2, k),
                    tailCompile(e, j))

            case LetTyp(_, be2) =>
                tailCompileBlockExp(be2, k)

            case Return(e) =>
                tailCompile(e, k)
        }

}
