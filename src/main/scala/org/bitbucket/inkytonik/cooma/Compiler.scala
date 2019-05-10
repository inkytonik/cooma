package org.bitbucket.inkytonik.cooma

object Compiler {

    import syntax.CoomaParserPrettyPrinter.show
    import syntax.CoomaParserSyntax._
    import Util.fresh

    val halt = x => AppC("halt", x)

    def compile(prog : Program) : Term =
        compileTop(prog.expression, halt, 0)

    def compileTop(exp : Expression, kappa : String => Term, arg : Int) : Term = {

        def compileArg(a : String, t : Type, e : Expression) : Term =
            t match {
                case IdnT(n) if (n == "Console") || (n == "Reader") =>
                    val x = fresh("x")
                    LetV(x, ArgV(arg),
                        LetV(a, CapV(n, x),
                            compileTop(e, kappa, arg + 1)))

                case StrT() =>
                    LetV(a, ArgV(arg),
                        compileTop(e, kappa, arg + 1))

                case _ =>
                    sys.error(s"compileArg: ${show(t)} arguments not supported")
            }

        exp match {
            case Fun(Vector(Argument(a, t)), e) =>
                compileArg(a, t, e)
            case Fun(Argument(a, t) +: as, e) =>
                compileArg(a, t, Fun(as, e))
            case _ =>
                compile(exp, kappa)
        }

    }

    def compile(exp : Expression, kappa : String => Term) : Term =
        exp match {
            case App(e, Vector()) =>
                val k = fresh("k")
                val x = fresh("x")
                val z = fresh("z")
                compile(e, y =>
                    LetV(z, UniV(),
                        LetC(k, x, kappa(x), AppF(y, k, z))))

            case App(e, Vector(a)) =>
                val k = fresh("k")
                val x = fresh("x")
                compile(e, y =>
                    compile(a, z =>
                        LetC(k, x, kappa(x), AppF(y, k, z))))

            case App(e, a +: as) =>
                compile(App(App(e, Vector(a)), as), kappa)

            case Block(es) =>
                sys.error("compile: block")

            case Fld(e, f) =>
                val x = fresh("x")
                compile(e, z =>
                    LetV(x, FldV(z, f), kappa(x)))

            case Fun(Vector(Argument(x, _)), e) =>
                val f = fresh("f")
                val k = fresh("k")
                LetV(f, FunV(k, x, compile(e, z => AppC(k, z))), kappa(f))

            case Fun(a +: as, e) =>
                compile(Fun(Vector(a), Fun(as, e)), kappa)

            case IdnUse(i) =>
                kappa(i)

            case Num(i) =>
                val x = fresh("x")
                LetV(x, IntV(i), kappa(x))

            case Row(Field(f, e)) =>
                val x = fresh("x")
                compile(e, z =>
                    LetV(x, RowV(FieldValue(f, z)), kappa(x)))

            case Str(s) =>
                val x = fresh("x")
                LetV(x, StrV(s), kappa(x))

            case Uni() =>
                val x = fresh("x")
                LetV(x, UniV(), kappa(x))
        }

}
