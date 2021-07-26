package org.bitbucket.inkytonik.cooma

object Desugar {

    import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
    import org.bitbucket.inkytonik.cooma.PrettyPrinter.show
    import org.bitbucket.inkytonik.cooma.SymbolTable.{Environment, intT, IntT, StrT}
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{everywhere, rewrite, rule}
    import org.bitbucket.inkytonik.kiama.relation.Tree
    import org.bitbucket.inkytonik.kiama.util.Messaging.{error, Messages, noMessages}
    import org.bitbucket.inkytonik.kiama.util.Positions

    def desugar[T <: ASTNode](root : T, env : Environment, positions : Positions) : Either[Messages, T] = {

        val tree = new Tree[ASTNode, T](root)
        val analyser = new SemanticAnalyser(tree, env)
        var messages = noMessages

        def appOp(n : Expression, rec : String, field : String, args : Seq[Expression]) =
            positions.dupPos(n, App(Sel(Idn(IdnUse(rec)), FieldUse(field)), args.toVector))

        def appFun(n : Expression, fun : String, args : Expression*) : Expression =
            positions.dupPos(n, App(Idn(IdnUse(fun)), args.toVector))

        def addError(node : ASTNode, label : String) : ASTNode = {
            messages = messages ++ error(node, label)
            node
        }

        def boolOp(n : Expression, field : String, args : Expression*) : Expression =
            appOp(n, "Booleans", field, args)

        def equalOp(n : Expression, t : Expression, l : Expression, r : Expression) : Expression =
            appFun(n, "equal", t, l, r)

        def notEqualOp(n : Expression, t : Expression, l : Expression, r : Expression) : Expression =
            boolOp(n, "not", equalOp(n, t, l, r))

        def intOp(n : Expression, field : String, args : Expression*) : Expression =
            appOp(n, "Ints", field, args)

        def strOp(n : Expression, field : String, args : Expression*) : Expression =
            appOp(n, "Strings", field, args)

        def vecOp(n : Expression, field : String, elemType : Expression, args : Expression*) : Expression =
            appOp(n, "Vectors", field, elemType +: args)

        val desugarOps =
            rule[ASTNode] {
                case n @ Abs(e) =>
                    analyser.tipe(e) match {
                        case Some(IntT()) =>
                            intOp(n, "abs", e)
                        case Some(StrT()) =>
                            strOp(n, "length", e)
                        case Some(VecNilT()) =>
                            Num(0)
                        case Some(VecT(t)) =>
                            vecOp(n, "length", t, e)
                        case Some(t) =>
                            addError(n, s"illegal |${show(t)}|, only Int, String or Vector supported")
                        case None =>
                            n
                    }

                case n @ Add(l, Concat(), r) =>
                    (analyser.tipe(l), analyser.tipe(r)) match {
                        case (Some(StrT()), _) =>
                            strOp(n, "concat", l, r)
                        case (Some(VecNilT()), Some(VecNilT() | VecT(_))) =>
                            r
                        case (Some(VecT(t)), _) =>
                            vecOp(n, "concat", t, l, r)
                        case (Some(t), Some(u)) =>
                            addError(n, s"illegal ${show(t)} ++ ${show(u)}, only String or Vector supported")
                        case _ =>
                            n
                    }
                case n @ Add(l, Plus(), r) =>
                    intOp(n, "add", l, r)
                case n @ Add(l, Minus(), r) =>
                    intOp(n, "sub", l, r)

                case n @ Eql(l, Equal(), r) =>
                    analyser.tipe(l) match {
                        case Some(lt @ VecNilT()) =>
                            analyser.tipe(r) match {
                                case Some(rt : VecT) =>
                                    equalOp(n, rt, l, r)
                                case Some(_) =>
                                    equalOp(n, lt, l, r)
                                case None =>
                                    n
                            }
                        case Some(lt) =>
                            equalOp(n, lt, l, r)
                        case None =>
                            n
                    }
                case n @ Eql(l, NotEqual(), r) =>
                    analyser.tipe(l) match {
                        case Some(lt @ VecNilT()) =>
                            analyser.tipe(r) match {
                                case Some(rt : VecT) =>
                                    notEqualOp(n, rt, l, r)
                                case Some(_) =>
                                    notEqualOp(n, lt, l, r)
                                case None =>
                                    n
                            }
                        case Some(lt) =>
                            notEqualOp(n, lt, l, r)
                        case None =>
                            n
                    }

                case n @ Exp(l, Expn(), r) =>
                    intOp(n, "pow", l, r)

                case n @ If(c, l, r) if analyser.tipe(l).isDefined =>
                    appFun(n, "ite", analyser.tipe(l).get, c, l, r)

                case n @ Ind(e, Index(), i) =>
                    analyser.tipe(e) match {
                        case Some(VecNilT()) =>
                            vecOp(n, "get", intT, e, i)
                        case Some(VecT(t)) =>
                            vecOp(n, "get", t, e, i)
                        case Some(t) =>
                            addError(n, s"illegal ${show(t)}!, only Vector supported")
                        case None =>
                            n
                    }

                case n @ Mul(l, Percent(), r) =>
                    intOp(n, "mod", l, r)
                case n @ Mul(l, Slash(), r) =>
                    intOp(n, "div", l, r)
                case n @ Mul(l, Star(), r) =>
                    intOp(n, "mul", l, r)

                case n @ And(l, AmpAmp(), r) =>
                    boolOp(n, "and", l, r)
                case n @ Or(l, BarBar(), r) =>
                    boolOp(n, "or", l, r)

                case n @ Pre(Bang(), e) =>
                    boolOp(n, "not", e)

                case n @ Rel(l, Gt(), r) =>
                    intOp(n, "gt", l, r)
                case n @ Rel(l, Gte(), r) =>
                    intOp(n, "gte", l, r)
                case n @ Rel(l, Lt(), r) =>
                    intOp(n, "lt", l, r)
                case n @ Rel(l, Lte(), r) =>
                    intOp(n, "lte", l, r)
            }

        val newRoot = rewrite(everywhere(desugarOps))(root)
        if (messages.isEmpty)
            Right(newRoot)
        else
            Left(messages)
    }

}
