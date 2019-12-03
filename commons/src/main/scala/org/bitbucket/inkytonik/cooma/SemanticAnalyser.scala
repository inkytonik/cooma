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
    val tree : Tree[ASTNode, ASTNode],
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

            case c : Case =>
                checkCase(c)

            case f @ Field(i, _) if isDuplicateField(f) =>
                error(f, s"duplicate field $i")

            case f @ FieldType(i, _) if isDuplicateFieldType(f) =>
                error(f, s"duplicate type field $i")

            case e : Expression =>
                checkExpressionType(e) ++
                    check(e) {
                        case App(f, as) =>
                            checkApplication(f, as)
                        case a @ Cat(l, r) =>
                            checkConcat(a, l, r)
                        case Mat(e, cs) =>
                            checkMatch(e, cs)
                        case Sel(e, f) =>
                            checkFieldUse(e, f)
                        case prm @ Prm(_, _) =>
                            checkPrimitive(prm)
                    }
        }

    def checkPrimitive(prm : Prm) : Messages = {
        primitivesTypesTable.get(prm.identifier) match {
            case Some(funT) =>
                if (prm.optExpressions.length != funT.optExpressions.length)
                    error(prm, s"primitive expects ${funT.optExpressions.length} arguments, provided ${prm.optExpressions.length}.")
                else
                    noMessages
            case None =>
                error(prm, s"primitive ${prm.identifier} not found")
        }
    }

    def checkExpressionType(e : Expression) : Messages =
        (unaliasedType(e), unalias(e, expectedType(e))) match {
            case (Some(t), Some(u)) if !subtype(t, u) =>
                error(e, s"expected ${show(u)}, got ${show(e)} of type ${show(t)}")
            case _ =>
                noMessages
        }

    def checkApplication(f : Expression, as : Vector[Expression]) : Messages =
        unaliasedType(f) match {
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

    def checkCase(c : Case) : Messages =
        c match {
            case tree.parent(tree.parent(m : Mat)) =>
                checkCaseDup(c, m) ++
                    checkCaseExpressionTypes(c, m) ++
                    checkCaseVariants(c, m)
            case _ =>
                sys.error(s"checkCase: can't find enclosing match")
        }

    def checkCaseDup(c : Case, m : Mat) : Messages =
        if (isDuplicateCase(c, m))
            error(c, s"duplicate case for variant ${c.identifier}")
        else
            noMessages

    def checkCaseExpressionTypes(c : Case, m : Mat) : Messages =
        matchType(m) match {
            case BadCases() =>
                error(c.expression, s"case expression types are not the same")
            case t =>
                noMessages
        }

    def checkCaseVariants(c : Case, m : Mat) : Messages =
        unaliasedType(m.expression) match {
            case Some(t @ VarT(fieldTypes)) =>
                fieldTypes.find(f => f.identifier == c.identifier) match {
                    case None =>
                        error(c, s"variant ${c.identifier} not present in matched type ${show(t)}")
                    case _ =>
                        noMessages
                }
            case _ =>
                noMessages
        }

    def checkConcat(e : Cat, l : Expression, r : Expression) : Messages = {
        checkRecordUse(l) ++
            checkRecordUse(r) ++
            ((unaliasedType(l), unaliasedType(r)) match {
                case (Some(RecT(rl)), Some(RecT(rr))) =>
                    val overlap = overlappingFields(rl, rr)
                    if (overlap.isEmpty)
                        noMessages
                    else {
                        val fieldsMsg = overlap.mkString(", ")
                        error(e, s"record concatenation has overlapping field(s) $fieldsMsg")
                    }
                case _ =>
                    noMessages
            })
    }

    def checkMatch(e : Expression, cs : Vector[CaseScope]) : Messages =
        checkMatchDiscType(e) ++
            checkMatchCaseNum(e, cs)

    def checkMatchDiscType(e : Expression) : Messages =
        unaliasedType(e) match {
            case Some(VarT(_)) | None =>
                noMessages
            case Some(t) =>
                error(e, s"match of non-variant type ${show(t)}")
        }

    def checkMatchCaseNum(e : Expression, cs : Vector[CaseScope]) : Messages =
        tipe(e) match {
            case Some(VarT(fields)) if fields.length != cs.length =>
                error(cs(0), s"expected ${fields.length} cases, got ${cs.length}")
            case _ =>
                noMessages
        }

    def checkRecordUse(e : Expression) : Messages =
        unaliasedType(e) match {
            case Some(RecT(_)) | None =>
                noMessages
            case Some(t) =>
                error(e, s"expected record type, got ${show(t)}")
        }

    def checkFieldUse(e : Expression, f : FieldUse) : Messages = {
        val i = f.identifier
        unaliasedType(e) match {
            case Some(t @ RecT(fields)) =>
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
                case _ : Body | _ : CaseScope | _ : Fun | _ : LetDef |
                    _ : LetVal | _ : REPLDef | _ : REPLExp |
                    _ : REPLVal =>
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
        case n : Case =>
            defineCaseValue(out(n), n)
        case n : Def =>
            defineFunction(out(n), n)
        case n : Val =>
            defineValue(out(n), n)
    }

    def defineArgument(e : Environment, a : Argument) : Environment =
        defineIfNew(e, a.idnDef.identifier, MultipleEntity(), ArgumentEntity(a))

    def defineCaseValue(e : Environment, c : Case) : Environment =
        defineIfNew(e, c.idnDef.identifier, MultipleEntity(), CaseValueEntity(c))

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

    /**
     * The "deepest" env of an expression, defined to be the env
     * of the plain expression, and the env of the body of a
     * block expression (recursively). Currently only used when
     * processing the predef.
     */
    lazy val deepEnv : Expression => Environment =
        attr {
            case Blk(b) => blockEnv(b)
            case e      => env(e)
        }

    lazy val blockEnv : BlockExp => Environment =
        attr {
            case LetDef(_, b) => blockEnv(b)
            case LetVal(_, b) => blockEnv(b)
            case Return(e)    => env(e)
        }

    lazy val entity : IdnUse => CoomaEntity =
        attr {
            case n =>
                lookup(env(n), n.identifier, UnknownEntity())
        }

    def isDuplicateCase(c : Case, m : Mat) =
        m.caseScopes.map(_.caseField.identifier).count(_ == c.identifier) > 1

    lazy val fieldNames : Rec => Vector[String] =
        attr {
            case Rec(fields) =>
                fields.map(_.identifier)
        }

    def isDuplicateField(f : Field) : Boolean =
        f match {
            case tree.parent.pair(Field(i, _), Rec(fields)) =>
                fieldCount(fields, i) > 1
            case tree.parent.pair(Field(i, _), _ : Var) =>
                false
        }

    def fieldCount(fields : Vector[Field], i : String) : Int =
        fields.map(_.identifier).count(_ == i)

    def isDuplicateFieldType(ft : FieldType) : Boolean =
        ft match {
            case tree.parent.pair(FieldType(i, _), RecT(fields)) =>
                fields.map(_.identifier).count(_ == i) > 1
            case tree.parent.pair(FieldType(i, _), VarT(fields)) =>
                fields.map(_.identifier).count(_ == i) > 1
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
                            Some(t)
                        else
                            Some(FunT(ts.drop(numArgs), t))
                    case _ =>
                        None
                }

            case Blk(b) =>
                blockTipe(b)

            case Cat(e1, e2) =>
                (tipe(e1), tipe(e2)) match {
                    case (Some(RecT(r1)), Some(RecT(r2))) =>
                        if (overlappingFields(r1, r2).isEmpty)
                            Some(RecT(r1 ++ r2))
                        else
                            None
                    case _ =>
                        None
                }

            case Fun(Arguments(as), e) =>
                tipe(e) match {
                    case Some(t) =>
                        Some(FunT(as.map(_.expression), t))
                    case None =>
                        None
                }

            case _ : FunT =>
                Some(TypT())

            case u @ Idn(IdnUse(x)) =>
                entityType(lookup(env(u), x, UnknownEntity()))

            case IntT() =>
                Some(TypT())

            case m : Mat =>
                matchType(m) match {
                    case OkCases(optType) =>
                        optType
                    case BadCases() =>
                        None
                }

            case _ : Num =>
                Some(IntT())

            case Prm(i, args) =>
                primitivesTypesTable.get(i) match {
                    case Some(FunT(ts, t)) =>
                        if (args.length == ts.length)
                            Some(t)
                        else
                            None
                    case _ =>
                        None
                }

            case Rec(fields) =>
                makeRow(fields).map(RecT)

            case _ : RecT =>
                Some(TypT())

            case Sel(r, FieldUse(f)) =>
                tipe(r) match {
                    case Some(RecT(fieldTypes)) =>
                        fieldTypes.find {
                            case FieldType(i, t) =>
                                i == f
                        } match {
                            case Some(FieldType(i, t)) =>
                                Some(t)
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

            case Var(field) =>
                makeRow(Vector(field)).map(VarT)

            case _ : VarT =>
                Some(TypT())
        }

    def makeRow(fields : Vector[Field]) : Option[Vector[FieldType]] = {
        val ts = fields.map(f => tipe(f.expression))
        if (ts contains None)
            None
        else {
            val us = ts.map(_.get)
            val ids = fields.map(_.identifier)
            val fieldTypes =
                (ids.zip(us)).map {
                    case (i, u) =>
                        FieldType(i, u)
                }
            Some(fieldTypes)
        }
    }

    val entityType : CoomaEntity => Option[Expression] =
        attr {
            case ArgumentEntity(Argument(_, t)) =>
                Some(t)
            case CaseValueEntity(tree.parent.pair(c, tree.parent(Mat(e, _)))) =>
                tipe(e) match {
                    case Some(VarT(r)) if tree.index(c) < r.length =>
                        Some(r(tree.index(c)).expression)
                    case _ =>
                        None
                }
            case FieldEntity(Field(_, e)) =>
                tipe(e)
            case FunctionEntity(Def(_, Body(Arguments(as), t, e))) =>
                Some(FunT(as.map(_.expression), t))
            case ValueEntity(Val(i, None, e)) =>
                tipe(e)
            case ValueEntity(Val(_, t, e)) =>
                t
            case _ =>
                None
        }

    abstract class MatchType
    case class BadCases() extends MatchType
    case class OkCases(optType : Option[Expression]) extends MatchType

    lazy val matchType : Mat => MatchType =
        attr {
            case m =>
                val caseTypes =
                    m.caseScopes.map {
                        case CaseScope(Case(_, _, e)) =>
                            unaliasedType(e)
                    }.distinct
                if (caseTypes contains None)
                    OkCases(None)
                else if (caseTypes.length > 1)
                    BadCases()
                else
                    OkCases(caseTypes(0))
        }

    val unaliasedType : Expression => Option[Expression] =
        attr {
            case e =>
                unalias(e, tipe(e))
        }

    def unalias(e : Expression, optType : Option[Expression]) : Option[Expression] =
        optType.flatMap(t => unalias(e, t))

    def unalias(e : Expression, t : Expression) : Option[Expression] =
        t match {
            case Idn(IdnUse(x)) =>
                lookup(env(e), x, UnknownEntity()) match {
                    case ValueEntity(Val(_, _, v)) if t != v =>
                        unalias(e, v)
                    case _ =>
                        None
                }
            case FunT(us, u) =>
                unaliasFunT(e, us, u)
            case RecT(fieldTypes) =>
                unaliasRecT(e, fieldTypes)
            case VarT(fieldTypes) =>
                unaliasVarT(e, fieldTypes)
            case _ =>
                Some(t)
        }

    def unaliases(e : Expression, ts : Vector[Expression]) : Option[Vector[Expression]] = {
        val us = ts.map(t => unalias(e, t))
        if (us.forall(_.isDefined))
            Some(us.map(_.get))
        else
            None
    }

    def unaliasFunT(e : Expression, ts : Vector[Expression], t : Expression) : Option[FunT] =
        unaliases(e, ts) match {
            case Some(us) =>
                unalias(e, t) match {
                    case Some(u) =>
                        Some(FunT(us, u))
                    case None =>
                        None
                }
            case None =>
                None
        }

    def unaliasFieldTypes(e : Expression, fts : Vector[FieldType]) : Option[Vector[FieldType]] = {
        val is = fts.map(_.identifier)
        val ts = fts.map(_.expression)
        unaliases(e, ts) match {
            case Some(us) =>
                Some(is.zip(us).map {
                    case (i, t) =>
                        FieldType(i, t)
                })
            case None =>
                None
        }
    }

    def unaliasRecT(e : Expression, fts : Vector[FieldType]) : Option[RecT] =
        unaliasFieldTypes(e, fts).map(RecT)

    def unaliasVarT(e : Expression, fts : Vector[FieldType]) : Option[VarT] =
        unaliasFieldTypes(e, fts).map(VarT)

    lazy val blockTipe : BlockExp => Option[Expression] =
        attr {
            case LetDef(_, b) => blockTipe(b)
            case LetVal(_, b) => blockTipe(b)
            case Return(e)    => tipe(e)
        }

    lazy val expectedType : Expression => Option[Expression] =
        attr {
            case tree.parent.pair(a, App(e, _)) if a ne e =>
                val argnum = tree.index(a) - 1
                tipe(e) match {
                    case Some(FunT(ts, _)) if ts.length > argnum =>
                        Some(ts(argnum))
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

            case tree.parent.pair(a, Prm(i, _)) =>
                val argnum = tree.index(a)
                primitivesTypesTable.get(i) match {
                    case Some(FunT(ts, _)) if ts.length > argnum =>
                        Some(ts(argnum))
                    case _ =>
                        None
                }

            case _ =>
                None
        }

    lazy val replType : REPLInput => Option[Expression] =
        attr {
            case REPLExp(e) =>
                tipe(e)
            case REPLDef(Def(_, Body(Arguments(as), t, e))) =>
                Some(FunT(as.map(_.expression), t))
            case REPLVal(Val(_, None, e)) =>
                tipe(e)
            case REPLVal(Val(_, t, _)) =>
                t
        }

}

object SemanticAnalysis {

    import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._

    def subtype(t : Expression, u : Expression) : Boolean =
        (t == u) ||
            ((t, u) match {
                case (RecT(tr), RecT(ur)) =>
                    ur.diff(tr).isEmpty
                case (VarT(tr), VarT(ur)) =>
                    tr.diff(ur).isEmpty
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
