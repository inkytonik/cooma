/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.cooma

object Compiler {

    import org.bitbucket.inkytonik.cooma.CoomaParserPrettyPrinter.show
    import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
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
        tailCompile(prog.expression, "halt")
    }

    def compileTop(exp : Expression, arg : Int) : Term = {

        def compileTopArg(a : String, t : Type, e : Expression) : Term =
            t match {
                case IdnT(n) if (n == "Console") || (n == "Reader") =>
                    val x = fresh("x")
                    LetV(x, ArgV(arg),
                        LetV(a, CapV(n, x),
                            compileTop(e, arg + 1)))

                case StrT() =>
                    LetV(a, ArgV(arg),
                        compileTop(e, arg + 1))

                case _ =>
                    sys.error(s"compileTopArg: ${show(t)} arguments not supported")
            }

        exp match {
            case Fun(Vector(Argument(a, t)), e) =>
                compileTopArg(a, t, e)
            case Fun(Argument(a, t) +: as, e) =>
                compileTopArg(a, t, Fun(as, e))
            case _ =>
                tailCompile(exp, "halt")
        }

    }

    def compile(exp : Expression, kappa : String => Term) : Term =
        exp match {
            case And(l, r) =>
                val x = fresh("x")
                compile(l, y =>
                    compile(r, z =>
                        LetV(x, AndV(y, z),
                            kappa(x))))

            case App(f, Vector(e)) =>
                val k = fresh("k")
                val x = fresh("x")
                compile(f, y =>
                    compile(e, z =>
                        LetC(k, x, kappa(x),
                            AppF(y, k, z))))

            case App(f, e +: es) =>
                compile(App(App(f, Vector(e)), es), kappa)

            case Blk(be) =>
                compileBlockExp(be, kappa)

            case Fun(Vector(Argument(x, t)), e) =>
                compileFun(x, t, e, kappa)

            case Fun(Argument(x, t) +: as, e) =>
                compileFun(x, t, Fun(as, e), kappa)

            case Var(i) =>
                kappa(i)

            case Num(i) =>
                val x = fresh("x")
                LetV(x, IntV(i),
                    kappa(x))

            case Row(fields) =>
                val x = fresh("x")
                compileRow(fields, fvs => LetV(x, RowV(fvs), kappa(x)))

            case Sel(r, f) =>
                val x = fresh("x")
                compile(r, z =>
                    LetV(x, SelV(z, f),
                        kappa(x)))

            case Str(s) =>
                val x = fresh("x")
                LetV(x, StrV(unescape(s.tail.init)),
                    kappa(x))

        }

    def compileFun(x : String, t : Type, e : Expression, kappa : String => Term) : Term =
        t match {
            case IdnT(n) if (n == "Console") || (n == "Reader") =>
                val f = fresh("f")
                val j = fresh("j")
                val y = fresh("y")
                LetV(f, FunV(j, y, LetV(x, CapV(n, y), tailCompile(e, j))),
                    kappa(f))

            case _ =>
                val f = fresh("f")
                val k = fresh("k")
                LetV(f, FunV(k, x, tailCompile(e, k)),
                    kappa(f))
        }

    def compileBlockExp(be : BlockExp, kappa : String => Term) : Term =
        be match {
            case LetFun(ds, be2) =>
                LetF(
                    ds.map(compileDef),
                    compileBlockExp(be2, kappa)
                )

            case LetVal(Val(x, e), be2) =>
                val j = fresh("j")
                LetC(j, x, compileBlockExp(be2, kappa),
                    tailCompile(e, j))

            case Return(e) =>
                compile(e, kappa)
        }

    def compileDef(fd : FunctionDefinition) : DefTerm =
        fd match {
            case Def(f, Argument(x, _) +: otherArgs, e) =>
                val k = fresh("k")
                val body =
                    if (otherArgs.isEmpty)
                        tailCompile(e, k)
                    else
                        tailCompile(Fun(otherArgs, e), k)
                DefTerm(f, k, x, body)
        }

    def compileRow(
        fields : Vector[Field],
        kappa : Vector[FieldValue] => Term
    ) : Term =
        fields match {
            case Field(f, e) +: t =>
                compile(e, z =>
                    compileRow(t, fvs => kappa(FieldValue(f, z) +: fvs)))

            case Vector() =>
                kappa(Vector())
        }

    def tailCompile(exp : Expression, k : String) : Term =
        exp match {
            case And(l, r) =>
                val x = fresh("x")
                compile(l, y =>
                    compile(r, z =>
                        LetV(x, AndV(y, z),
                            AppC(k, x))))

            case App(e, Vector(a)) =>
                compile(e, x1 =>
                    compile(a, x2 =>
                        AppF(x1, k, x2)))

            case App(e, a +: as) =>
                tailCompile(App(App(e, Vector(a)), as), k)

            case Blk(be) =>
                tailCompileBlockExp(be, k)

            case Fun(Vector(Argument(x, t)), e) =>
                tailCompileFun(x, t, e, k)

            case Fun(Argument(x, t) +: as, e) =>
                tailCompileFun(x, t, Fun(as, e), k)

            case Fun(a +: as, e) =>
                tailCompile(Fun(Vector(a), Fun(as, e)), k)

            case Var(x) =>
                AppC(k, x)

            case Num(i) =>
                val x = fresh("x")
                LetV(x, IntV(i),
                    AppC(k, x))

            case Row(fields) =>
                val x = fresh("x")
                compileRow(fields, fvs => LetV(x, RowV(fvs), AppC(k, x)))

            case Sel(e, f) =>
                val x = fresh("x")
                compile(e, z =>
                    LetV(x, SelV(z, f),
                        AppC(k, x)))

            case Str(s) =>
                val x = fresh("x")
                LetV(x, StrV(unescape(s.tail.init)),
                    AppC(k, x))
        }

    def tailCompileFun(x : String, t : Type, e : Expression, k : String) : Term =
        t match {
            case IdnT(n) if (n == "Console") || (n == "Reader") =>
                val f = fresh("f")
                val j = fresh("j")
                val y = fresh("y")
                LetV(f, FunV(j, y, LetV(x, CapV(n, y), tailCompile(e, j))),
                    AppC(k, f))

            case _ =>
                val f = fresh("f")
                val j = fresh("j")
                LetV(f, FunV(j, x, tailCompile(e, j)),
                    AppC(k, f))
        }

    def tailCompileBlockExp(be : BlockExp, k : String) : Term =
        be match {
            case LetFun(ds, be2) =>
                LetF(
                    ds.map(compileDef),
                    tailCompileBlockExp(be2, k)
                )

            case LetVal(Val(x, e), be2) =>
                val j = fresh("j")
                LetC(j, x, tailCompileBlockExp(be2, k),
                    tailCompile(e, j))

            case Return(e) =>
                tailCompile(e, k)
        }

}
