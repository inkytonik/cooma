package org.bitbucket.inkytonik.cooma

trait Compiler {

    self : Backend =>

    import org.bitbucket.inkytonik.cooma.CoomaParserPrettyPrinter.show
    import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
    import org.bitbucket.inkytonik.cooma.Primitives._
    import org.bitbucket.inkytonik.cooma.Util.{fresh, resetFresh, unescape}

    /**
     * Compile a program that will run as a command with
     * user-supplied command-line arguments.
     */
    def compileCommand(prog : Program) : Term = {
        resetFresh()
        compileBlockExp(prog.blockExp, z => appC("$halt", z), top = true)
    }

    /**
     * Compile a program that is evaluated as an expression
     * with no command-line arguments.
     */
    def compileStandalone(prog : Program) : Term = {
        resetFresh()
        compileBlockExp(prog.blockExp, z => appC("$halt", z), top = true)
    }

    /**
     * Name of capability type?
     */
    def isCapabilityName(n : String) : Boolean =
        (n == "Writer") || (n == "Reader") || (n == "ReaderWriter")

    /**
     * Case class and map that stores primitives metadata.
     */
    case class PrimitiveMeta(prm : Primitive)

    val primitivesTable = Map(
        "Equal" -> PrimitiveMeta(equalP),
        "IntAbs" -> PrimitiveMeta(intBinP(ABS)),
        "IntAdd" -> PrimitiveMeta(intBinP(ADD)),
        "IntSub" -> PrimitiveMeta(intBinP(SUB)),
        "IntMul" -> PrimitiveMeta(intBinP(MUL)),
        "IntDiv" -> PrimitiveMeta(intBinP(DIV)),
        "IntPow" -> PrimitiveMeta(intBinP(POW)),
        "IntGt" -> PrimitiveMeta(intRelP(GT)),
        "IntGte" -> PrimitiveMeta(intRelP(GTE)),
        "IntLt" -> PrimitiveMeta(intRelP(LT)),
        "IntLte" -> PrimitiveMeta(intRelP(LTE)),
        "StrConcat" -> PrimitiveMeta(stringP(CONCAT)),
        "StrLength" -> PrimitiveMeta(stringP(LENGTH)),
        "StrSubstr" -> PrimitiveMeta(stringP(SUBSTR)),
        "SelectItemVector" -> PrimitiveMeta(selectItemVector()),
        "AppendItemVector" -> PrimitiveMeta(appendItemVector()),
        "PrependItemVector" -> PrimitiveMeta(prependItemVector()),
        "PutItemVector" -> PrimitiveMeta(putItemVector()),
        "VectorLength" -> PrimitiveMeta(vectorLength()),
        "SliceVector" -> PrimitiveMeta(sliceVector()),
        "ConcatVector" -> PrimitiveMeta(concatVector()),
        "MapVector" -> PrimitiveMeta(mapVector()),
        "Exception" -> PrimitiveMeta(exception())
    )

    /**
     * Compile an expression to produce its value via the halt
     * continuation.
     */
    def compileHalt(exp : Expression) : Term =
        compile(exp, z => appC("$halt", z))

    def compileTop(exp : Expression, nArg : Int) : Term = {

        def compileTopArg(a : String, t : Expression, e : Expression) : Term = {

            def compileCapArg(n : String) : Term = {
                val x = fresh("x")
                letV(x, prmV(argumentP(nArg), Vector()),
                    letV(a, prmV(capabilityP(n), Vector(x)),
                        compileTop(e, nArg + 1)))
            }

            t match {
                case ReaderT()       => compileCapArg("Reader")
                case ReaderWriterT() => compileCapArg("ReaderWriter")
                case WriterT()       => compileCapArg("Writer")

                case StrT() =>
                    letV(a, prmV(argumentP(nArg), Vector()),
                        compileTop(e, nArg + 1))

                case _ =>
                    sys.error(s"compileTopArg: ${show(t)} arguments not supported")
            }

        }

        exp match {
            case Fun(Arguments(Vector()), e) =>
                compileHalt(e)
            case Fun(Arguments(Vector(Argument(IdnDef(a), t, _))), e) =>
                compileTopArg(a, t, e)
            case Fun(Arguments(Argument(IdnDef(a), t, _) +: as), e) =>
                compileTopArg(a, t, Fun(Arguments(as), e))
            case _ =>
                compileHalt(exp)
        }
    }

    def mkPrimField(fieldName : String, argTypes : Vector[Expression], primName : String) : Field = {
        mkPrimFieldArgNames(fieldName, (1 to argTypes.length).map(i => s"arg$i").zip(argTypes), primName)
    }

    def mkPrimFieldArgNames(fieldName : String, argNames : Seq[(String, Expression)], primName : String) : Field = {
        val args = argNames.map { case (n, t) => Argument(IdnDef(n), t, None) }
        val params = argNames.map { case (n, _) => Idn(IdnUse(n)) }.toVector
        Field(fieldName, Fun(Arguments(args.toVector), Prm(primName, params)))
    }

    def mkVectorPrimFieldArgNames(fieldName : String, argNames : Seq[(String, Expression)], primName : String) : Field = {
        mkPrimFieldArgNames(fieldName, Vector(("t", TypT()), ("v", VecT(Idn(IdnUse("t"))))) ++ argNames, primName)
    }

    def mkInt1PrimField(fieldName : String, primName : String) : Field =
        mkPrimField(fieldName, Vector(IntT()), primName)

    def mkInt2PrimField(fieldName : String, primName : String) : Field =
        mkPrimField(fieldName, Vector(IntT(), IntT()), primName)

    def mkStr1PrimField(fieldName : String, primName : String) : Field =
        mkPrimField(fieldName, Vector(StrT()), primName)

    def mkStr2PrimField(fieldName : String, primName : String) : Field =
        mkPrimField(fieldName, Vector(StrT(), StrT()), primName)

    def mkStrIntPrimField(fieldName : String, primName : String) : Field =
        mkPrimField(fieldName, Vector(StrT(), IntT()), primName)

    val equal =
        Fun(
            Arguments(Vector(
                Argument(IdnDef("t"), TypT(), None),
                Argument(IdnDef("l"), Idn(IdnUse("t")), None),
                Argument(IdnDef("r"), Idn(IdnUse("t")), None)
            )),
            Prm("Equal", Vector(
                Idn(IdnUse("t")),
                Idn(IdnUse("l")),
                Idn(IdnUse("r"))
            ))
        )

    val ints =
        Rec(Vector(
            mkInt1PrimField("abs", "IntAbs"),
            mkInt2PrimField("add", "IntAdd"),
            mkInt2PrimField("div", "IntDiv"),
            mkInt2PrimField("mul", "IntMul"),
            mkInt2PrimField("pow", "IntPow"),
            mkInt2PrimField("sub", "IntSub"),
            mkInt2PrimField("lt", "IntLt"),
            mkInt2PrimField("lte", "IntLte"),
            mkInt2PrimField("gt", "IntGt"),
            mkInt2PrimField("gte", "IntGte")
        ))

    val strings =
        Rec(Vector(
            mkStr2PrimField("concat", "StrConcat"),
            mkStr1PrimField("length", "StrLength"),
            mkStrIntPrimField("substr", "StrSubstr")
        ))

    //    val vectors =
    //        Rec(Vector(
    //            mkVectorPrimFieldArgNames("length", Vector(), "VectorLength"),
    //            mkVectorPrimFieldArgNames("get", Vector(("i", IntT())), "SelectItemVector"),
    //            mkVectorPrimFieldArgNames("append", Vector(("e", Idn(IdnUse("t")))), "AppendItemVector"),
    //            mkVectorPrimFieldArgNames("prepend", Vector(("e", Idn(IdnUse("t")))), "PrependItemVector"),
    //            mkVectorPrimFieldArgNames("put", Vector(("i", IntT()), ("e", Idn(IdnUse("t")))), "PutItemVector"),
    //            mkVectorPrimFieldArgNames("slice", Vector(("i", IntT()), ("j", IntT())), "SliceVector"),
    //            mkVectorPrimFieldArgNames("concat", Vector(("vr", VecT(Idn(IdnUse("t"))))), "ConcatVector"),
    //        ))

    def compile(exp : Expression, kappa : String => Term) : Term =
        exp match {
            case App(f, Vector()) =>
                compile(App(f, Vector(Uni())), kappa)

            case App(f, Vector(a)) =>
                val k = fresh("k")
                val r = fresh("r")
                compile(f, y =>
                    compile(a, z =>
                        letC(k, r, kappa(r),
                            appF(y, k, z))))

            case App(f, a +: as) =>
                compile(App(App(f, Vector(a)), as), kappa)

            case Blk(be) =>
                compileBlockExp(be, kappa, false)

            case Cat(r1, r2) =>
                val r = fresh("r")
                compile(r1, y =>
                    compile(r2, z =>
                        letV(r, prmV(concatP(), Vector(y, z)),
                            kappa(r))))

            case False() =>
                compile(Var(Field("False", Uni())), kappa)

            case Fun(Arguments(Vector()), e) =>
                compileFun("_", UniT(), e, kappa)

            case Fun(Arguments(Vector(Argument(IdnDef(x), t, _))), e) =>
                compileFun(x, t, e, kappa)

            case Fun(Arguments(Argument(IdnDef(x), t, _) +: as), e) =>
                compileFun(x, t, Fun(Arguments(as), e), kappa)

            case Idn(IdnUse(i)) =>
                kappa(i)

            case Mat(e, cs) =>
                compileMatch(e, cs, kappa)

            case Num(n) =>
                val i = fresh("i")
                letV(i, intV(n),
                    kappa(i))

            case Prm(p, args) =>
                val r = fresh("r")
                compilePrimArgs(args, cArgs => letV(r, prmV(primitivesTable(p).prm, cArgs),
                    kappa(r)))

            case Rec(fields) =>
                val r = fresh("r")
                compileRec(fields, fvs => letV(r, recV(fvs), kappa(r)))

            case Sel(r, FieldUse(s)) =>
                val f = fresh("f")
                compile(r, z =>
                    letV(f, prmV(selectP(), Vector(z, s)),
                        kappa(f)))

            case Str(l) =>
                val s = fresh("s")
                letV(s, strV(unescape(l.tail.init)),
                    kappa(s))

            case True() =>
                compile(Var(Field("True", Uni())), kappa)

            case Uni() =>
                val u = fresh("u")
                letV(u, recV(Vector()),
                    kappa(u))

            case v @ Var(field) =>
                val r = fresh("r")
                compile(field.expression, z =>
                    letV(r, varV(field.identifier, z),
                        kappa(r)))

            case VecLit(VecElems(e)) =>
                val v = fresh("v")
                compilePrimArgs(e, cElems =>
                    letV(v, vecV(cElems), kappa(v)))

            // Types erase to unit
            case IsType() =>
                compile(Uni(), kappa)
        }

    object IsType {
        def unapply(e : Expression) : Boolean =
            e match {
                case BoolT() | ReaderT() | ReaderWriterT() | WriterT() |
                    _ : FunT | _ : IntT | _ : RecT | _ : StrT | _ : TypT |
                    _ : UniT | _ : VarT | _ : VecT =>
                    true
                case _ =>
                    false
            }
    }

    def compileCapFun(n : String, x : String, e : Expression, kappa : String => Term) : Term = {
        val f = fresh("f")
        val j = fresh("k")
        val y = fresh("y")
        letV(f, funV(j, y,
            letV(x, prmV(capabilityP(n), Vector(y)),
                tailCompile(e, j))),
            kappa(f))
    }

    def compileFun(x : String, t : Expression, e : Expression, kappa : String => Term) : Term = {
        val f = fresh("f")
        val j = fresh("k")
        letV(f, funV(j, x, tailCompile(e, j)),
            kappa(f))
    }

    def compileBlockExp(be : BlockExp, kappa : String => Term, top : Boolean) : Term =
        be match {
            case BlkDef(ds, be2) =>
                letF(
                    ds.defs.map(compileDef),
                    compileBlockExp(be2, kappa, top)
                )

            case BlkLet(Let(_, IdnDef(x), _, e), be2) =>
                val j = fresh("k")
                letC(j, x, compileBlockExp(be2, kappa, top),
                    tailCompile(e, j))

            case Return(e) =>
                top match {
                    case true => compileTop(e, 0)
                    case _    => compile(e, kappa)
                }
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
                compileDef(Def(IdnDef(f), Body(Arguments(Vector(Argument(IdnDef("_"), UniT(), None))), t, e)))

            case Def(IdnDef(f), Body(Arguments(Argument(IdnDef(x), _, None) +: otherArgs), _, e)) =>
                defTerm(f, k, x, compileDefBody(otherArgs, e, k))
        }
    }

    def compileMatch(e : Expression, cs : Vector[Case], kappa : String => Term) : Term = {
        val cks = cs.map(c => (c, fresh("k")))
        val caseTerms = cks.map { case (c, k) => caseTerm(c.identifier, k) }
        compile(e, z =>
            cks.foldLeft(casV(z, caseTerms)) {
                case (t, (Case(_, IdnDef(xi), ei), ki)) =>
                    letC(ki, xi, compile(ei, zi => kappa(zi)),
                        t)
            })
    }

    def compileRec(
        fields : Vector[Field],
        kappa : Vector[FieldValue] => Term
    ) : Term =
        fields match {
            case Field(f, e) +: t =>
                compile(e, z =>
                    compileRec(t, fvs => kappa(fieldValue(f, z) +: fvs)))

            case Vector() =>
                kappa(Vector())
        }

    def compilePrimArgs(
        args : Vector[Expression],
        kappa : Vector[String] => Term
    ) : Term =
        args match {
            case e +: t =>
                compile(e, argE =>
                    compilePrimArgs(t, argT => kappa(argE +: argT)))

            case Vector() =>
                kappa(Vector())
        }

    def tailCompile(exp : Expression, k : String) : Term =
        exp match {
            case App(f, Vector()) =>
                tailCompile(App(f, Vector(Uni())), k)

            case App(f, Vector(a)) =>
                compile(f, y =>
                    compile(a, z =>
                        appF(y, k, z)))

            case App(f, a +: as) =>
                tailCompile(App(App(f, Vector(a)), as), k)

            case Blk(be) =>
                tailCompileBlockExp(be, k)

            case Cat(r1, r2) =>
                val r = fresh("r")
                compile(r1, y =>
                    compile(r2, z =>
                        letV(r, prmV(concatP(), Vector(y, z)),
                            appC(k, r))))

            case False() =>
                tailCompile(Var(Field("False", Uni())), k)

            case Fun(Arguments(Vector()), e) =>
                tailCompileFun("_", UniT(), e, k)

            case Fun(Arguments(Vector(Argument(IdnDef(x), t, _))), e) =>
                tailCompileFun(x, t, e, k)

            case Fun(Arguments(Argument(IdnDef(x), t, _) +: as), e) =>
                tailCompileFun(x, t, Fun(Arguments(as), e), k)

            case Fun(Arguments(a +: as), e) =>
                tailCompile(Fun(Arguments(Vector(a)), Fun(Arguments(as), e)), k)

            case Idn(IdnUse(x)) =>
                appC(k, x)

            case Mat(e, cs) =>
                tailCompileMatch(e, cs, k)

            case Num(n) =>
                val i = fresh("i")
                letV(i, intV(n),
                    appC(k, i))

            case Prm(p, args) =>
                val r = fresh("r")
                compilePrimArgs(args, cArgs =>
                    letV(r, prmV(primitivesTable(p).prm, cArgs),
                        appC(k, r)))

            case Rec(fields) =>
                val r = fresh("r")
                compileRec(fields, fvs => letV(r, recV(fvs), appC(k, r)))

            case Sel(r, FieldUse(s)) =>
                val f = fresh("f")
                compile(r, z =>
                    letV(f, prmV(selectP(), Vector(z, s)),
                        appC(k, f)))

            case Str(l) =>
                val s = fresh("s")
                letV(s, strV(unescape(l.tail.init)),
                    appC(k, s))

            case True() =>
                tailCompile(Var(Field("True", Uni())), k)

            case Uni() =>
                val u = fresh("u")
                letV(u, recV(Vector()),
                    appC(k, u))

            case Var(field) =>
                val r = fresh("r")
                compile(field.expression, z =>
                    letV(r, varV(field.identifier, z),
                        appC(k, r)))

            case VecLit(e) =>
                val v = fresh("v")
                compilePrimArgs(e.optExpressions, cElems =>
                    letV(v, vecV(cElems), appC(k, v)))

            // Types erase to unit
            case IsType() =>
                tailCompile(Uni(), k)
        }

    def tailCompileCapFun(n : String, x : String, e : Expression, k : String) : Term = {
        val f = fresh("f")
        val j = fresh("k")
        val y = fresh("y")
        letV(f, funV(j, y,
            letV(x, prmV(capabilityP(n), Vector(y)),
                tailCompile(e, j))),
            appC(k, x))
    }

    def tailCompileFun(x : String, t : Expression, e : Expression, k : String) : Term = {
        val f = fresh("f")
        val j = fresh("k")
        letV(f, funV(j, x, tailCompile(e, j)),
            appC(k, f))
    }

    def tailCompileBlockExp(be : BlockExp, k : String) : Term =
        be match {
            case BlkDef(ds, be2) =>
                letF(
                    ds.defs.map(compileDef),
                    tailCompileBlockExp(be2, k)
                )

            case BlkLet(Let(_, IdnDef(x), _, e), be2) =>
                val j = fresh("k")
                letC(j, x, tailCompileBlockExp(be2, k),
                    tailCompile(e, j))

            case Return(e) =>
                tailCompile(e, k)
        }

    def tailCompileMatch(e : Expression, cs : Vector[Case], k : String) : Term = {
        val cks = cs.map(c => (c, fresh("k")))
        val caseTerms = cks.map { case (c, k) => caseTerm(c.identifier, k) }
        compile(e, z =>
            cks.foldLeft(casV(z, caseTerms)) {
                case (t, (Case(vi, IdnDef(xi), ei), ki)) =>
                    letC(ki, xi, tailCompile(ei, k),
                        t)
            })
    }

}
