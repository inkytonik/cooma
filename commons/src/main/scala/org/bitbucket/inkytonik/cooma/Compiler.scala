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
    import org.bitbucket.inkytonik.kiama.relation.Bridge

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
        compile(exp, z => appC(Bridge(exp), "$halt", z))

    def compileTop(exp : Expression, nArg : Int) : Term = {

        def compileTopArg(arg : Argument, a : String, t : Expression, e : Expression) : Term = {

            def compileCapArg(n : List[String]) : Term = {
                val x = fresh("x")
                def aux(n : List[String], prev : String) : Term = {
                    val c0 = fresh("c")
                    n match {
                        case hd :: Nil =>
                            letV(Bridge(arg), c0, prmV(capabilityP(hd), Vector(x)),
                                letV(Bridge(arg), a, prmV(recConcatP(), Vector(prev, c0)),
                                    compileTop(e, nArg + 1)))
                        case hd :: (tl @ _ :: _) =>
                            val c1 = fresh("c")
                            letV(Bridge(arg), c0, prmV(capabilityP(hd), Vector(x)),
                                letV(Bridge(arg), c1, prmV(recConcatP(), Vector(prev, c0)),
                                    aux(tl, c1)))
                        case Nil =>
                            sys.error("compileCapArg: unexpected Nil")
                    }
                }
                n match {
                    case hd :: Nil =>
                        letV(Bridge(arg), x, prmV(argumentP(nArg), Vector()),
                            letV(Bridge(arg), a, prmV(capabilityP(hd), Vector(x)), compileTop(e, nArg + 1)))
                    case hd :: tl =>
                        val c = fresh("c")
                        letV(Bridge(arg), x, prmV(argumentP(nArg), Vector()),
                            letV(Bridge(arg), c, prmV(capabilityP(hd), Vector(x)), aux(tl, c)))
                    case Nil =>
                        sys.error("compileCapArg: unexpected Nil")
                }
            }

            t match {
                case StrT() =>
                    letV(Bridge(arg), a, prmV(argumentP(nArg), Vector()),
                        compileTop(e, nArg + 1))
                case t =>
                    def aux(t : Expression) : List[String] =
                        t match {
                            case Cat(e1, e2) =>
                                aux(e1) ::: aux(e2)
                            case CapT(name) =>
                                name.productPrefix.init :: Nil
                            case t =>
                                sys.error(s"compileTopArg: ${show(t)} arguments not supported")
                        }
                    compileCapArg(aux(t))
            }

        }

        exp match {
            case Fun(Arguments(Vector()), e) =>
                compileHalt(e)
            case Fun(Arguments(Vector(arg @ Argument(IdnDef(a), t, _))), e) =>
                compileTopArg(arg, a, t, e)
            case Fun(Arguments((arg @ Argument(IdnDef(a), t, _)) +: as), e) =>
                compileTopArg(arg, a, t, Fun(Arguments(as), e))
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

    val equalPrim =
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

    val intPrims =
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

    val stringPrims =
        Rec(Vector(
            mkStr2PrimField("concat", "StrConcat"),
            mkStr1PrimField("length", "StrLength"),
            mkStrIntPrimField("substr", "StrSubstr")
        ))

    /**
     * Short-hand for compiling where the source expression and the expression
     * being compiled are the same.
     */
    def compile(exp : Expression, kappa : String => Term) : Term =
        compile(exp, exp, kappa)

    /**
     * Compile `exp`. The compiled code will be related to the `source` node. In many
     * cases these will be the same, but where we desguar `source` will be the
     * original node, whereas `exp` will be the desugared version.
     */
    def compile(source : Expression, exp : Expression, kappa : String => Term) : Term =
        exp match {
            case App(f, Vector()) =>
                compile(exp, App(f, Vector(Uni())), kappa)

            case App(f, Vector(a)) =>
                val k = fresh("k")
                val r = fresh("r")
                compile(f, y =>
                    compile(a, z =>
                        letC(Bridge(source), k, r, kappa(r),
                            appF(Bridge(source), y, k, z))))

            case App(f, a +: as) =>
                compile(exp, App(App(f, Vector(a)), as), kappa)

            case Blk(be) =>
                compileBlockExp(be, kappa)

            case Booleans() =>
                compile(exp, booleans, kappa)

            case Cat(r1, r2) =>
                val r = fresh("r")
                compile(r1, y =>
                    compile(r2, z =>
                        letV(Bridge(source), r, prmV(recConcatP(), Vector(y, z)),
                            kappa(r))))

            case Eql() =>
                compile(exp, equalPrim, kappa)

            case False() =>
                compile(exp, Var(Field("False", Uni())), kappa)

            case Fun(Arguments(Vector()), e) =>
                compileFun(exp, "_", UniT(), e, kappa)

            case Fun(Arguments(Vector(Argument(IdnDef(x), t, _))), e) =>
                compileFun(exp, x, t, e, kappa)

            case Fun(Arguments(Argument(IdnDef(x), t, _) +: as), e) =>
                compileFun(exp, x, t, Fun(Arguments(as), e), kappa)

            case Idn(IdnUse(i)) =>
                kappa(i)

            case Ints() =>
                compile(exp, intPrims, kappa)

            case Mat(e, cs) =>
                compileMatch(exp, e, cs, kappa)

            case Num(n) =>
                val i = fresh("i")
                letV(Bridge(source), i, intV(n),
                    kappa(i))

            case Prm(p, args) =>
                val r = fresh("r")
                compilePrimArgs(args, cArgs => letV(Bridge(source), r, prmV(primitivesTable(p).prm, cArgs),
                    kappa(r)))

            case Rec(fields) =>
                val r = fresh("r")
                compileRec(fields, fvs => letV(Bridge(source), r, recV(fvs), kappa(r)))

            case Sel(r, FieldUse(s)) =>
                val f = fresh("f")
                compile(r, z =>
                    letV(Bridge(source), f, prmV(recSelectP(), Vector(z, s)),
                        kappa(f)))

            case Str(l) =>
                val s = fresh("s")
                letV(Bridge(source), s, strV(unescape(l.tail.init)),
                    kappa(s))

            case Strings() =>
                compile(exp, stringPrims, kappa)

            case True() =>
                compile(exp, Var(Field("True", Uni())), kappa)

            case Uni() =>
                val u = fresh("u")
                letV(Bridge(source), u, recV(Vector()),
                    kappa(u))

            case v @ Var(field) =>
                val r = fresh("r")
                compile(source, field.expression, z =>
                    letV(Bridge(source), r, varV(field.identifier, z),
                        kappa(r)))

            // Types erase to unit
            case IsType() =>
                compile(exp, Uni(), kappa)

            case _ =>
                sys.error(s"compile: unexpected expression $exp")
        }

    object IsType {
        def unapply(e : Expression) : Boolean =
            e match {
                case BoolT() | CapT(_) |
                    _ : FunT | _ : IntT | _ : RecT | _ : StrT | _ : TypT |
                    _ : UniT | _ : VarT =>
                    true
                case _ =>
                    false
            }
    }

    def compileFun(exp : Expression, x : String, t : Expression, e : Expression, kappa : String => Term) : Term = {
        val f = fresh("f")
        val j = fresh("k")
        letV(Bridge(exp), f, funV(j, x, tailCompile(e, j)),
            kappa(f))
    }

    def compileBlockExp(be : BlockExp, kappa : String => Term) : Term =
        be match {
            case BlkDef(ds, be2) =>
                letF(
                    Bridge(be),
                    ds.defs.map(compileDef),
                    compileBlockExp(be2, kappa)
                )

            case BlkLet(Let(_, IdnDef(x), _, e), be2) =>
                val j = fresh("k")
                letC(Bridge(be), j, x, compileBlockExp(be2, kappa),
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
                defTerm(Bridge(fd), f, k, x, compileDefBody(otherArgs, e, k))

            case _ =>
                sys.error(s"compileDef: unexpected definition $fd")
        }
    }

    def compileMatch(exp : Expression, e : Expression, cs : Vector[Case], kappa : String => Term) : Term = {
        val cks = cs.map(c => (c, fresh("k")))
        val caseTerms = cks.map { case (c, k) => caseTerm(Bridge(c), c.identifier, k) }
        compile(e, z =>
            cks.foldLeft(casV(Bridge(exp), z, caseTerms)) {
                case (t, (Case(_, IdnDef(xi), ei), ki)) =>
                    letC(Bridge(ei), ki, xi, compile(ei, zi => kappa(zi)),
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
        tailCompile(exp, exp, k)

    def tailCompile(source : Expression, exp : Expression, k : String) : Term =
        exp match {
            case App(f, Vector()) =>
                tailCompile(exp, App(f, Vector(Uni())), k)

            case App(f, Vector(a)) =>
                compile(f, y =>
                    compile(a, z =>
                        appF(Bridge(source), y, k, z)))

            case App(f, a +: as) =>
                tailCompile(exp, App(App(f, Vector(a)), as), k)

            case Blk(be) =>
                tailCompileBlockExp(be, k)

            case Booleans() =>
                tailCompile(booleans, k)

            case Cat(r1, r2) =>
                val r = fresh("r")
                compile(r1, y =>
                    compile(r2, z =>
                        letV(Bridge(source), r, prmV(recConcatP(), Vector(y, z)),
                            appC(Bridge(source), k, r))))

            case Eql() =>
                tailCompile(exp, equalPrim, k)

            case False() =>
                tailCompile(exp, Var(Field("False", Uni())), k)

            case Fun(Arguments(Vector()), e) =>
                tailCompileFun(exp, "_", UniT(), e, k)

            case Fun(Arguments(Vector(Argument(IdnDef(x), t, _))), e) =>
                tailCompileFun(exp, x, t, e, k)

            case Fun(Arguments(Argument(IdnDef(x), t, _) +: as), e) =>
                tailCompileFun(exp, x, t, Fun(Arguments(as), e), k)

            case Fun(Arguments(a +: as), e) =>
                tailCompile(exp, Fun(Arguments(Vector(a)), Fun(Arguments(as), e)), k)

            case Idn(IdnUse(x)) =>
                appC(Bridge(source), k, x)

            case Ints() =>
                tailCompile(exp, intPrims, k)

            case Mat(e, cs) =>
                tailCompileMatch(exp, e, cs, k)

            case Num(n) =>
                val i = fresh("i")
                letV(Bridge(source), i, intV(n),
                    appC(Bridge(source), k, i))

            case Prm(p, args) =>
                val r = fresh("r")
                compilePrimArgs(args, cArgs =>
                    letV(Bridge(source), r, prmV(primitivesTable(p).prm, cArgs),
                        appC(Bridge(source), k, r)))

            case Rec(fields) =>
                val r = fresh("r")
                compileRec(fields, fvs => letV(Bridge(source), r, recV(fvs), appC(Bridge(source), k, r)))

            case Sel(r, FieldUse(s)) =>
                val f = fresh("f")
                compile(r, z =>
                    letV(Bridge(source), f, prmV(recSelectP(), Vector(z, s)),
                        appC(Bridge(source), k, f)))

            case Str(l) =>
                val s = fresh("s")
                letV(Bridge(source), s, strV(unescape(l.tail.init)),
                    appC(Bridge(source), k, s))

            case Strings() =>
                tailCompile(exp, stringPrims, k)

            case True() =>
                tailCompile(exp, Var(Field("True", Uni())), k)

            case Uni() =>
                val u = fresh("u")
                letV(Bridge(source), u, recV(Vector()),
                    appC(Bridge(source), k, u))

            case Var(field) =>
                val r = fresh("r")
                compile(source, field.expression, z =>
                    letV(Bridge(source), r, varV(field.identifier, z),
                        appC(Bridge(source), k, r)))

            // Types erase to unit
            case IsType() =>
                tailCompile(exp, Uni(), k)

            case _ =>
                sys.error(s"tailCompile: unexpected expression $exp")
        }

    def tailCompileFun(exp : Expression, x : String, t : Expression, e : Expression, k : String) : Term = {
        val f = fresh("f")
        val j = fresh("k")
        letV(Bridge(exp), f, funV(j, x, tailCompile(e, j)),
            appC(Bridge(exp), k, f))
    }

    def tailCompileBlockExp(be : BlockExp, k : String) : Term =
        be match {
            case BlkDef(ds, be2) =>
                letF(
                    Bridge(be),
                    ds.defs.map(compileDef),
                    tailCompileBlockExp(be2, k)
                )

            case BlkLet(Let(_, IdnDef(x), _, e), be2) =>
                val j = fresh("k")
                letC(Bridge(be), j, x, tailCompileBlockExp(be2, k),
                    tailCompile(e, j))

            case Return(e) =>
                tailCompile(e, k)
        }

    def tailCompileMatch(exp : Expression, e : Expression, cs : Vector[Case], k : String) : Term = {
        val cks = cs.map(c => (c, fresh("k")))
        val caseTerms = cks.map { case (c, k) => caseTerm(Bridge(c), c.identifier, k) }
        compile(e, z =>
            cks.foldLeft(casV(Bridge(exp), z, caseTerms)) {
                case (t, (Case(vi, IdnDef(xi), ei), ki)) =>
                    letC(Bridge(ei), ki, xi, tailCompile(ei, k),
                        t)
            })
    }

}
