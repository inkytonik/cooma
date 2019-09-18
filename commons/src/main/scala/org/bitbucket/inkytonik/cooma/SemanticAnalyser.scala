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

import org.bitbucket.inkytonik.cooma.CoomaParserSyntax.{ASTNode, Program}
import org.bitbucket.inkytonik.cooma.SymbolTable._
import org.bitbucket.inkytonik.kiama.attribution.Attribution
import org.bitbucket.inkytonik.kiama.relation.Tree

class SemanticAnalyser(
    tree : Tree[ASTNode, ASTNode],
    rootenv : Environment = predef
) extends Attribution {

    import org.bitbucket.inkytonik.kiama.==>
    import org.bitbucket.inkytonik.kiama.attribution.Decorators
    import org.bitbucket.inkytonik.kiama.util.Messaging.{check, collectMessages, error, Messages, noMessages}
    import org.bitbucket.inkytonik.cooma.CoomaParserPrettyPrinter.show
    import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
    import org.bitbucket.inkytonik.cooma.SemanticAnalysis.subtype

    val decorators = new Decorators(tree)
    import decorators._

    lazy val errors : Messages =
        collectMessages(tree) {
            case d @ IdnDef(i) =>
                lookup(env(d), i, UnknownEntity()) match {
                    case MultipleEntity() =>
                        error(d, s"$i is declared more than once")
                    case _ =>
                        noMessages
                }

            case u @ IdnUse(i) if entity(u) == UnknownEntity() =>
                error(u, s"$i is not declared")

            case f @ Field(i, _) if isDuplicateField(f) =>
                error(f, s"duplicate field name $i")

            case f @ FieldType(i, _) if isDuplicateFieldType(f) =>
                error(f, s"duplicate type field name $i")

            case e : Expression =>
                checkExpressionType(e) ++
                    check(e) {
                        case App(f, as) =>
                            checkApplication(f, as)
                        case a @ Cat(l, r) =>
                            checkConcat(a, l, r)
                        case Sel(e, f) =>
                            checkFieldUse(e, f)
                    }
        }

    def checkExpressionType(e : Expression) : Messages =
        (tipe(e), expectedType(e)) match {
            case (Some(t), Some(u)) if !subtype(t, u) =>
                error(e, s"expected ${show(u)}, got ${show(e)} of type ${show(t)}")
            case _ =>
                noMessages
        }

    def checkApplication(f : Expression, as : Vector[Expression]) : Messages =
        tipe(f) match {
            case Some(FunT(ps, _)) =>
                if (ps.length < as.length)
                    ps.length match {
                        case 0 =>
                            error(as(0), s"expected no arguments, got ${as.length}")
                        case 1 =>
                            error(as(0), s"expected up to one argument, got ${as.length}")
                        case n =>
                            error(as(0), s"expected up to $n arguments, got ${as.length}")
                    }
                else
                    noMessages
            case Some(t) =>
                error(f, s"application of non-function type ${show(t)}")
            case _ =>
                noMessages
        }

    def checkConcat(e : Cat, l : Expression, r : Expression) : Messages = {
        checkRecordUse(l) ++
            checkRecordUse(r) ++
            ((tipe(l), tipe(r)) match {
                case (Some(RecT(Row(rl))), Some(RecT(Row(rr)))) =>
                    val overlap = overlappingFields(rl, rr)
                    if (overlap.isEmpty)
                        noMessages
                    else {
                        val fieldsMsg = overlap.mkString(", ")
                        error(e, s"record concatenation has overlapping fields $fieldsMsg")
                    }
                case _ =>
                    noMessages
            })
    }

    def checkRecordUse(e : Expression) : Messages =
        tipe(e) match {
            case Some(RecT(_)) | None =>
                noMessages
            case Some(t) =>
                error(e, s"expected record type, got ${show(t)}")
        }

    def checkFieldUse(e : Expression, f : FieldUse) : Messages = {
        val i = f.identifier
        tipe(e) match {
            case Some(t @ RecT(Row(fields))) =>
                if (fields.map(_.identifier) contains i)
                    noMessages
                else
                    error(f, s"$i is not a field of record type ${show(t)}")
            case Some(t) =>
                error(f, s"selection of $i field from non-record type ${show(t)}")
            case None =>
                noMessages
        }
    }

    object Scope {
        def unapply(n : ASTNode) : Boolean =
            n match {
                case _ : Body | _ : Fun | _ : LetFun | _ : LetVal |
                    _ : REPLDef | _ : REPLExpression | _ : REPLVal =>
                    true
                case _ =>
                    false
            }
    }

    lazy val defenv : Chain[Environment] =
        chain(defenvin, defenvout)

    def defenvin(in : ASTNode => Environment) : ASTNode ==> Environment = {
        case _ : Program | _ : REPLInput =>
            rootenv
        case n @ Scope() =>
            enter(in(n))
    }

    def defenvout(out : ASTNode => Environment) : ASTNode ==> Environment = {
        case n @ Scope() =>
            leave(out(n))
        case n : Argument =>
            defineArgument(out(n), n)
        case n : Def =>
            defineFunction(out(n), n)
        case n : Val =>
            defineValue(out(n), n)
    }

    def defineArgument(e : Environment, a : Argument) : Environment =
        defineIfNew(e, a.idnDef.identifier, MultipleEntity(), ArgumentEntity(a))

    def defineFunction(e : Environment, d : Def) : Environment =
        defineIfNew(e, d.idnDef.identifier, MultipleEntity(), FunctionEntity(d))

    def defineValue(e : Environment, v : Val) : Environment =
        defineIfNew(e, v.idnDef.identifier, MultipleEntity(), ValueEntity(v))

    lazy val env : ASTNode => Environment =
        attr {
            case tree.lastChild.pair(_ : Program | Scope(), c) =>
                defenv(c)
            case tree.parent.pair(e, p : Val) =>
                leave(env(p))
            case tree.parent(p) =>
                env(p)
        }

    lazy val entity : IdnUse => CoomaEntity =
        attr {
            case n =>
                lookup(env(n), n.identifier, UnknownEntity())
        }

    lazy val fieldNames : Rec => Vector[String] =
        attr {
            case Rec(fields) =>
                fields.map(_.identifier)
        }

    lazy val fieldTypeNames : Row => Vector[String] =
        attr {
            case Row(fields) =>
                fields.map(_.identifier)
        }

    lazy val isDuplicateField : Field => Boolean =
        attr {
            case tree.parent.pair(Field(i, _), r : Rec) =>
                fieldNames(r).count(_ == i) > 1
        }

    lazy val isDuplicateFieldType : FieldType => Boolean =
        attr {
            case tree.parent.pair(FieldType(i, _), r : Row) =>
                fieldTypeNames(r).count(_ == i) > 1
        }

    def overlappingFields(
        r1 : Vector[FieldType],
        r2 : Vector[FieldType]
    ) : Set[String] = {
        val r1names = r1.map(_.identifier).toSet
        val r2names = r2.map(_.identifier).toSet
        r1names.intersect(r2names)
    }

    lazy val tipe : Expression => Option[Expression] =
        attr {
            case App(f, as) =>
                tipe(f) match {
                    case Some(FunT(ts, t)) =>
                        val numArgs = as.length
                        if (numArgs == ts.length)
                            unalias(t)
                        else
                            unaliasFun(ts.drop(numArgs), t)
                    case _ =>
                        None
                }

            case Blk(b) =>
                blockTipe(b)

            case Cat(e1, e2) =>
                (tipe(e1), tipe(e2)) match {
                    case (Some(RecT(Row(r1))), Some(RecT(Row(r2)))) =>
                        if (overlappingFields(r1, r2).isEmpty)
                            Some(RecT(Row(r1 ++ r2)))
                        else
                            None
                    case _ =>
                        None
                }

            case Fun(Arguments(as), e) =>
                tipe(e) match {
                    case Some(t) =>
                        unaliasFun(as.map(_.expression), t)
                    case None =>
                        None
                }

            case _ : FunT =>
                Some(TypT())

            case u @ Idn(IdnUse(x)) =>
                entityType(lookup(env(u), x, UnknownEntity()))

            case IntT() =>
                Some(TypT())

            case _ : Num =>
                Some(IntT())

            case Rec(fields) =>
                makeRow(fields).map(RecT)

            case _ : RecT =>
                Some(TypT())

            case Sel(r, FieldUse(f)) =>
                tipe(r) match {
                    case Some(RecT(Row(fieldTypes))) =>
                        fieldTypes.find {
                            case FieldType(i, t) =>
                                i == f
                        } match {
                            case Some(FieldType(i, t)) =>
                                unalias(t)
                            case None =>
                                None
                        }
                    case _ =>
                        None
                }

            case _ : Str =>
                Some(StrT())

            case StrT() =>
                Some(TypT())

            case TypT() =>
                Some(TypT())

            case Uni() =>
                Some(UniT())

            case UniT() =>
                Some(TypT())

            case Var(fields) =>
                makeRow(fields).map(VarT)

            case _ : VarT =>
                Some(TypT())
        }

    def makeRow(fields : Vector[Field]) : Option[Row] = {
        val ts = fields.map(f => tipe(f.expression))
        if (ts contains None)
            None
        else {
            unaliases(ts.map(_.get)) match {
                case Some(us) =>
                    val ids = fields.map(_.identifier)
                    val fieldTypes =
                        (ids.zip(us)).map {
                            case (i, u) =>
                                FieldType(i, u)
                        }
                    Some(Row(fieldTypes))
                case None =>
                    None
            }
        }
    }

    val entityType : CoomaEntity => Option[Expression] =
        attr {
            case ArgumentEntity(Argument(_, t)) =>
                unalias(t)
            case FieldEntity(Field(_, e)) =>
                tipe(e)
            case FunctionEntity(Def(_, Body(Arguments(as), t, e))) =>
                unaliasFun(as.map(_.expression), t)
            case ValueEntity(Val(_, e)) =>
                tipe(e)
            case _ =>
                None
        }

    def unalias(t : Expression) : Option[Expression] =
        t match {
            case Idn(u @ IdnUse(x)) =>
                lookup(env(u), x, UnknownEntity()) match {
                    case ValueEntity(Val(_, v)) if t != v =>
                        unalias(v)
                    case _ =>
                        None
                }
            case _ =>
                Some(t)
        }

    def unaliases(ts : Vector[Expression]) : Option[Vector[Expression]] = {
        val us = ts.map(unalias)
        if (us.forall(_.isDefined))
            Some(us.map(_.get))
        else
            None
    }

    def unaliasFun(ts : Vector[Expression], t : Expression) : Option[Expression] =
        unaliases(ts) match {
            case Some(us) =>
                unalias(t) match {
                    case Some(u) =>
                        Some(FunT(us, u))
                    case None =>
                        None
                }
            case None =>
                None
        }

    lazy val blockTipe : BlockExp => Option[Expression] =
        attr {
            case LetFun(_, b) => blockTipe(b)
            case LetVal(_, b) => blockTipe(b)
            case Return(e)    => tipe(e)
        }

    lazy val expectedType : Expression => Option[Expression] =
        attr {
            case tree.parent.pair(a, App(e, _)) if a ne e =>
                val argnum = tree.index(a) - 1
                tipe(e) match {
                    case Some(FunT(ts, _)) if ts.length > argnum =>
                        unalias(ts(argnum))
                    case _ =>
                        None
                }

            case tree.parent(_ : Argument) =>
                Some(TypT())

            case tree.parent.pair(a, Body(_, t, e)) if a eq e =>
                Some(t)

            case tree.parent.pair(a, Body(_, t, _)) if a eq t =>
                Some(TypT())

            case tree.parent(_ : FieldType) =>
                Some(TypT())

            case tree.parent(_ : FunT) =>
                Some(TypT())

            case _ =>
                None
        }

    lazy val replType : REPLInput => Option[Expression] =
        attr {
            case REPLExpression(e) =>
                tipe(e)
            case REPLDef(Def(_, Body(Arguments(as), t, e))) =>
                unaliasFun(as.map(_.expression), t)
            case REPLVal(v) =>
                tipe(v.expression)
        }

}

object SemanticAnalysis {

    import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._

    def subtype(t : Expression, u : Expression) : Boolean =
        (t == u) ||
            ((t, u) match {
                case (RecT(Row(tr)), RecT(Row(tu))) =>
                    tu.diff(tr).isEmpty
                case (FunT(ts, t), FunT(us, u)) =>
                    subtypes(us, ts) && subtype(t, u)
                case _ =>
                    false
            })

    def subtypes(ts : Vector[Expression], us : Vector[Expression]) : Boolean =
        (ts.length == us.length) &&
            (ts.zip(us).forall {
                case (t, u) =>
                    subtype(t, u)
            })

}
