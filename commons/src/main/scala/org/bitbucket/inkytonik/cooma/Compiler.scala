/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

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
        "StrSubstr" -> PrimitiveMeta(stringP(SUBSTR))
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
                case ReaderT() => compileCapArg("Reader")
                case WriterT() => compileCapArg("Writer")

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

    val and =
        Fun(
            Arguments(Vector(
                Argument(IdnDef("l"), BoolT(), None),
                Argument(IdnDef("r"), BoolT(), None)
            )),
            Mat(
                Idn(IdnUse("l")),
                Vector(
                    Case("False", IdnDef("_"), False()),
                    Case("True", IdnDef("_"), Idn(IdnUse("r")))
                )
            )
        )

    val not =
        Fun(
            Arguments(Vector(
                Argument(IdnDef("b"), BoolT(), None)
            )),
            Mat(
                Idn(IdnUse("b")),
                Vector(
                    Case("False", IdnDef("_"), True()),
                    Case("True", IdnDef("_"), False())
                )
            )
        )

    val or =
        Fun(
            Arguments(Vector(
                Argument(IdnDef("l"), BoolT(), None),
                Argument(IdnDef("r"), BoolT(), None)
            )),
            Mat(
                Idn(IdnUse("l")),
                Vector(
                    Case("False", IdnDef("_"), Idn(IdnUse("r"))),
                    Case("True", IdnDef("_"), True())
                )
            )
        )

    val booleans =
        Rec(Vector(
            Field("and", and),
            Field("not", not),
            Field("or", or)
        ))

    def mkPrimField(fieldName : String, argTypes : Vector[Expression], primName : String) : Field = {
        val argNames = (1 to argTypes.length).map(i => s"arg$i")
        val args = argNames.zip(argTypes).map { case (n, t) => Argument(IdnDef(n), t, None) }
        val params = argNames.map(n => Idn(IdnUse(n))).toVector
        Field(fieldName, Fun(Arguments(args.toVector), Prm(primName, params)))
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
                compileBlockExp(be, kappa)

            case Booleans() =>
                compile(booleans, kappa)

            case Cat(r1, r2) =>
                val r = fresh("r")
                compile(r1, y =>
                    compile(r2, z =>
                        letV(r, prmV(recConcatP(), Vector(y, z)),
                            kappa(r))))

            case Eql() =>
                compile(equal, kappa)

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

            case Ints() =>
                compile(ints, kappa)

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
                    letV(f, prmV(recSelectP(), Vector(z, s)),
                        kappa(f)))

            case Str(l) =>
                val s = fresh("s")
                letV(s, strV(unescape(l.tail.init)),
                    kappa(s))

            case Strings() =>
                compile(strings, kappa)

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

            // Types erase to unit
            case IsType() =>
                compile(Uni(), kappa)

            case _ =>
                sys.error(s"compile: unexpected expression $exp")
        }

    object IsType {
        def unapply(e : Expression) : Boolean =
            e match {
                case BoolT() | ReaderT() | WriterT() |
                    _ : FunT | _ : IntT | _ : RecT | _ : StrT | _ : TypT |
                    _ : UniT | _ : VarT =>
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

    def compileBlockExp(be : BlockExp, kappa : String => Term) : Term =
        be match {
            case BlkDef(ds, be2) =>
                letF(
                    ds.defs.map(compileDef),
                    compileBlockExp(be2, kappa)
                )

            case BlkLet(Let(_, IdnDef(x), _, e), be2) =>
                val j = fresh("k")
                letC(j, x, compileBlockExp(be2, kappa),
                    tailCompile(e, j))

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
                compileDef(Def(IdnDef(f), Body(Arguments(Vector(Argument(IdnDef("_"), UniT(), None))), t, e)))

            case Def(IdnDef(f), Body(Arguments(Argument(IdnDef(x), _, None) +: otherArgs), _, e)) =>
                defTerm(f, k, x, compileDefBody(otherArgs, e, k))

            case _ =>
                sys.error(s"compileDef: unexpected definition $fd")
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

            case _ =>
                sys.error(s"compileRec: unexpected fields $fields")
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

            case _ =>
                sys.error(s"compilePrimArgs: unexpected fields $args")
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

            case Booleans() =>
                tailCompile(booleans, k)

            case Cat(r1, r2) =>
                val r = fresh("r")
                compile(r1, y =>
                    compile(r2, z =>
                        letV(r, prmV(recConcatP(), Vector(y, z)),
                            appC(k, r))))

            case Eql() =>
                tailCompile(equal, k)

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

            case Ints() =>
                tailCompile(ints, k)

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
                    letV(f, prmV(recSelectP(), Vector(z, s)),
                        appC(k, f)))

            case Str(l) =>
                val s = fresh("s")
                letV(s, strV(unescape(l.tail.init)),
                    appC(k, s))

            case Strings() =>
                tailCompile(strings, k)

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

            // Types erase to unit
            case IsType() =>
                tailCompile(Uni(), k)

            case _ =>
                sys.error(s"tailCompile: unexpected expression $exp")
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
