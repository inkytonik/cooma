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

import org.bitbucket.inkytonik.cooma.CoomaParserSyntax.ASTNode
import org.bitbucket.inkytonik.cooma.SymbolTable._
import org.bitbucket.inkytonik.kiama.attribution.Attribution
import org.bitbucket.inkytonik.kiama.relation.Tree

import scala.annotation.tailrec
import scala.collection.immutable.SeqMap

class SemanticAnalyser(
    val tree : Tree[ASTNode, ASTNode],
    predefStaticEnv : Environment = rootenv()
) extends Attribution {

    import org.bitbucket.inkytonik.kiama.==>
    import org.bitbucket.inkytonik.kiama.attribution.Decorators
    import org.bitbucket.inkytonik.kiama.rewriting.Cloner.deepclone
    import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{everywhere, rewrite, rule}
    import org.bitbucket.inkytonik.kiama.util.Messaging.{check, collectMessages, error, Messages, noMessages}
    import org.bitbucket.inkytonik.cooma.PrettyPrinter.show
    import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
    import org.bitbucket.inkytonik.cooma.SymbolTable.capabilityTypeNames

    val decorators = new Decorators(tree)
    import decorators._

    lazy val errors : Messages =
        collectMessages(tree) {
            case i @ IdnDef(s) if isPrimitiveTypeName(s) =>
                error(i, s"$s is a reserved primitive type name")

            case tree.parent.pair(d @ IdnDef(i), p) =>
                lookup(env(p), i, UnknownEntity()) match {
                    case MultipleEntity() =>
                        error(d, s"re-declaration of $i")
                    case _ =>
                        noMessages
                }

            case c : Case =>
                checkCase(c)

            case f @ Field(i, _) if isDuplicateField(f) =>
                error(f, s"duplicate field $i")

            case f @ FieldType(i, _) if isDuplicateFieldType(f) =>
                error(f, s"duplicate type field $i")

            case e : Expression =>
                checkMainProgram(e) ++
                    checkExpressionType(e) ++
                    check(e) {
                        case App(f, as) =>
                            checkApplication(f, as)
                        case a @ Cat(l, r) =>
                            checkConcat(a, l, r)
                        case PrimitiveType() =>
                            noMessages
                        case Idn(u @ IdnUse(i)) if entity(u) == UnknownEntity() =>
                            error(u, s"$i is not declared")
                        case m : Mat =>
                            checkMatch(m)
                        case Sel(e, f) =>
                            checkFieldUse(e, f)
                        case prm @ Prm(_, _) =>
                            checkPrimitive(prm)
                        case vec : Vec =>
                            checkVectorElements(vec)
                    }
        }

    def checkExpressionType(e : Expression) : Messages =
        (tipe(e), expectedType(e)) match {
            case (Some(t), Some(u)) if !subtype(t, u) =>
                error(e, s"expected ${show(u)}, got ${show(e)} of type ${show(t)}")
            case (a, b) =>
                noMessages
        }

    def checkApplication(f : Expression, as : Vector[Expression]) : Messages =
        tipe(f) match {
            case Some(FunT(ArgumentTypes(ps), _)) =>
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
            case None =>
                noMessages
        }

    def checkCase(c : Case) : Messages =
        c match {
            case tree.parent(m : Mat) =>
                checkCaseDup(c, m) ++
                    checkCaseVariants(c, m)
            case _ =>
                sys.error(s"checkCase: can't find enclosing match")
        }

    def checkCaseDup(c : Case, m : Mat) : Messages =
        if (isDuplicateCase(c, m))
            error(c, s"duplicate case for variant ${c.identifier}")
        else
            noMessages

    def checkCaseExpressionTypes(m : Mat) : Messages =
        matchType(m) match {
            case NoBound => error(m, s"case expressions must be of a common type")
            case _       => noMessages
        }

    def checkCaseVariants(c : Case, m : Mat) : Messages =
        tipe(m.expression) match {
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
        def checkOverlapping(rl : Vector[FieldType], rr : Vector[FieldType]) : Messages = {
            val overlap = overlappingFields(rl, rr)
            if (overlap.isEmpty)
                noMessages
            else {
                val fieldsMsg = overlap.mkString(", ")
                error(e, s"record concatenation has overlapping field(s) $fieldsMsg")
            }
        }
        (tipe(l), tipe(r)) match {
            case (None, _) | (_, None) =>
                // error occurred elsewhere
                noMessages
            case (Some(RecT(rl)), Some(RecT(rr))) =>
                // term-level concatenation
                checkOverlapping(rl, rr)
            case (Some(RecT(_)), Some(t)) =>
                // term-level concatenation, invalid right operand
                error(r, s"expected record, got ${show(r)} of type ${show(t)}")
            case (Some(TypT()), Some(TypT())) =>
                // type-level concatenation
                (unalias(e, l), unalias(e, r)) match {
                    case (None, _) | (_, None) =>
                        noMessages
                    case (Some(RecT(rl)), Some(RecT(rr))) =>
                        checkOverlapping(rl, rr)
                    case (Some(RecT(_)), Some(t)) =>
                        error(r, s"expected record type, got ${show(r)} of type ${show(t)}")
                    case (Some(t), _) =>
                        error(l, s"expected record type, got ${show(l)} of type ${show(t)}")
                }
            case (Some(TypT()), Some(t)) =>
                // type-level concatenation, invalid right operand
                error(r, s"expected record type, got ${show(r)} of type ${show(t)}")
            case (Some(t), _) =>
                error(l, s"expected record or record type, got ${show(l)} of type ${show(t)}")
        }
    }

    def checkFieldUse(e : Expression, f : FieldUse) : Messages = {
        val i = f.identifier
        tipe(e) match {
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

    def checkMatch(m : Mat) : Messages = {
        val e = m.expression
        val cs = m.cases
        checkMatchCaseNum(e, cs) ++ checkMatchDiscType(e) ++ checkCaseExpressionTypes(m)
    }

    def checkMatchCaseNum(e : Expression, cs : Vector[Case]) : Messages =
        tipe(e) match {
            case Some(VarT(fields)) if fields.length != cs.length =>
                error(cs(0), s"expected ${fields.length} cases, got ${cs.length}")
            case _ =>
                noMessages
        }

    def checkMatchDiscType(e : Expression) : Messages =
        tipe(e) match {
            case Some(VarT(_)) | None =>
                noMessages
            case Some(t) =>
                error(e, s"match of non-variant type ${show(t)}")
        }

    def checkMainProgram(e : Expression) : Messages =
        e match {
            case tree.parent(_ : Program) =>
                e match {
                    case Fun(Arguments(as), _) =>
                        as.flatMap(checkMainArgument)
                    case _ =>
                        noMessages
                }
            case _ =>
                noMessages
        }

    def checkMainArgument(arg : Argument) : Messages = {
        def aux(t : Expression) : Boolean =
            t match {
                case App(Idn(IdnUse("Database")), Vector(RecT(tables))) =>
                    tables.forall {
                        case FieldType(_, App(Idn(IdnUse("Table")), Vector(RecT(headers)))) =>
                            headers.forall {
                                case FieldType(_, t) =>
                                    def isBaseType(s : String) : Boolean =
                                        s == "Boolean" || s == "Int" || s == "String"
                                    t match {
                                        case Idn(IdnUse(s)) => isBaseType(s)
                                        case App(Idn(IdnUse("Option")), Vector(Idn(IdnUse(s)))) => isBaseType(s)
                                        case _ => false
                                    }
                                case _ => false
                            }
                        case _ => false
                    }
                case App(Idn(IdnUse("HttpServer")), Vector(RecT(endpoints))) =>
                    // TODO: check endpoints
                    true
                case StrT() =>
                    true
                case Idn(IdnUse(name)) if capabilityTypeNames(name) =>
                    true
                case Cat(l, r) =>
                    aux(l) && aux(r)
                case _ =>
                    false
            }
        if (aux(arg.expression))
            noMessages
        else
            error(arg.expression, "illegal main program argument type")
    }

    def checkPrimitive(prm : Prm) : Messages = {
        val tipe = userPrimitiveType(prm.userPrimitive)
        val numArgs = prm.optExpressions.length
        val expectedNumArgs = tipe.argumentTypes.optArgumentTypes.length
        if (numArgs != expectedNumArgs)
            error(prm, s"primitive ${show(prm.userPrimitive)} expects $expectedNumArgs arguments got ${numArgs}")
        else
            noMessages
    }

    val vecLubType : VecElems => BoundResult =
        attr {
            case VecElems(elems) =>
                getLub(elems)
        }

    def checkVectorElements(vec : Vec) : Messages =
        vecLubType(vec.vecElems) match {
            case Bound(_) | IgnoreBound => noMessages
            case NoBound                => error(vec, "Vector elements must be of a common type")
        }

    object Scope {
        def unapply(n : ASTNode) : Boolean =
            n match {
                case _ : Body | _ : Case | _ : Fun | _ : BlkDef |
                    _ : BlkLet | _ : REPLDef | _ : REPLExp |
                    _ : REPLLet =>
                    true
                case _ =>
                    false
            }
    }

    lazy val env : Chain[Environment] =
        chain(envin, envout)

    def envin(in : ASTNode => Environment) : ASTNode ==> Environment = {
        case _ : Program | _ : REPLInput =>
            predefStaticEnv

        // Bodies of defs get all the bindings of the Defs group
        case tree.parent.pair(_ : Body, tree.parent(p : Defs)) =>
            enter(env.out(p))

        case n @ Scope() =>
            enter(in(n))
    }

    def isWildcard(i : String) : Boolean =
        i == "_"

    def envout(out : ASTNode => Environment) : ASTNode ==> Environment = {
        // Avoid cycle where Defs group bindings involve themselves
        case tree.parent.pair(_ : Body, Def(d, _)) =>
            env.out(d)

        case n @ Scope() =>
            leave(out(n))

        case n @ Argument(IdnDef(i), _, _) if !isWildcard(i) =>
            defineIfNew(out(n), i, MultipleEntity(), ArgumentEntity(n))

        case tree.parent.pair(n @ IdnDef(i), c : Case) if !isWildcard(i) =>
            defineIfNew(out(n), i, MultipleEntity(), CaseValueEntity(c))

        case tree.parent.pair(n @ IdnDef(i), d : Def) if !isWildcard(i) =>
            defineIfNew(out(n), i, MultipleEntity(), FunctionEntity(d))

        case n @ Let(_, IdnDef(i), _, _) if !isWildcard(i) =>
            defineIfNew(out(n), i, MultipleEntity(), LetEntity(n))
    }

    /**
     * The "deepest" env of an expression, defined to be the env
     * of the plain expression, and the env of the body of a
     * block expression (recursively). Used when extracting the
     * predef env.
     */
    lazy val deepEnv : Expression => Environment =
        attr {
            case Blk(b) => blockEnv(b)
            case e      => env(e)
        }

    lazy val blockEnv : BlockExp => Environment =
        attr {
            case BlkDef(_, b) => blockEnv(b)
            case BlkLet(_, b) => blockEnv(b)
            case Return(e)    => env(e)
        }

    lazy val entity : IdnUse => CoomaEntity =
        attr {
            case n =>
                lookup(env(n), n.identifier, UnknownEntity())
        }

    lazy val defentity : IdnDef => CoomaEntity =
        attr {
            case tree.parent(p) =>
                p match {
                    case decl : Argument => ArgumentEntity(decl)
                    case decl : Case     => CaseValueEntity(decl)
                    case decl : Def      => FunctionEntity(decl)
                    case decl : Let      => LetEntity(decl)
                    case _               => UnknownEntity()
                }
            case n =>
                sys.error(s"defentity: unexpected IdnDef $n")
        }

    def isDuplicateCase(c : Case, m : Mat) =
        m.cases.map(_.identifier).count(_ == c.identifier) > 1

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
            case _ =>
                sys.error(s"isDuplicateField: unexpected field $f")
        }

    def fieldCount(fields : Vector[Field], i : String) : Int =
        fields.map(_.identifier).count(_ == i)

    def isDuplicateFieldType(ft : FieldType) : Boolean =
        ft match {
            case tree.parent.pair(FieldType(i, _), RecT(fields)) =>
                fields.map(_.identifier).count(_ == i) > 1
            case tree.parent.pair(FieldType(i, _), VarT(fields)) =>
                fields.map(_.identifier).count(_ == i) > 1
            case _ =>
                sys.error(s"isDuplicateFieldType: unexpected field type $ft")
        }

    def overlappingFields(
        r1 : Vector[FieldType],
        r2 : Vector[FieldType]
    ) : Set[String] = {
        val r1names = r1.map(_.identifier).toSet
        val r2names = r2.map(_.identifier).toSet
        r1names.intersect(r2names)
    }

    def argsToArgTypes(as : Vector[Argument]) : Vector[ArgumentType] =
        as.map(a => ArgumentType(Some(a.idnDef), a.expression))

    lazy val tipe : Expression => Option[Expression] =
        attr {
            case _ : Abs =>
                Some(intT)

            case Add(l, _, _) =>
                tipe(l) match {
                    case Some(TypT()) =>
                        Some(typT)
                    case _ =>
                        Some(intT)
                }

            case n : App =>
                appResType(n)

            case Blk(b) =>
                blockTipe(b)

            case _ : And | _ : Eql | _ : Or | _ : Rel =>
                Some(boolT)

            case n @ Cat(e1, e2) =>
                (tipe(e1), tipe(e2)) match {
                    case (Some(RecT(r1)), Some(RecT(r2))) =>
                        if (overlappingFields(r1, r2).isEmpty)
                            Some(RecT(r1 ++ r2))
                        else
                            None
                    case (Some(TypT()), Some(TypT())) =>
                        (unalias(n, e1), unalias(n, e2)) match {
                            case (Some(RecT(r1)), Some(RecT(r2))) =>
                                if (overlappingFields(r1, r2).isEmpty)
                                    Some(typT)
                                else
                                    None
                            case _ =>
                                None
                        }
                    case _ =>
                        None
                }

            case Fun(Arguments(as), e) =>
                tipe(e) match {
                    case Some(t) =>
                        unaliasFunT(e, argsToArgTypes(as), t)
                    case None =>
                        None
                }

            case _ : FunT =>
                Some(typT)

            case PrimitiveType() =>
                Some(typT)

            case u @ Idn(IdnUse(x)) =>
                entityType(lookup(env(u), x, UnknownEntity()))

            case If(_, l, r) =>
                deepclone(tipe(l))

            case Ind(e, Index(), i) =>
                tipe(e) match {
                    case Some(VecT(t)) =>
                        Some(deepclone(t))
                    case _ =>
                        None
                }

            case m : Mat =>
                matchType(m) match {
                    case Bound(t) => Some(t)
                    case _        => None
                }

            case _ : Mul =>
                Some(intT)

            case _ : Num =>
                Some(intT)

            case Pre(op, _) =>
                op match {
                    case Bang() => Some(boolT)
                }

            case n @ Prm(p, as) =>
                val ftype = userPrimitiveType(p)
                appType(n, Some(ftype), as) match {
                    case (_, t) =>
                        unalias(n, t)
                    case _ =>
                        None
                }

            case Rec(fields) =>
                makeRow(fields).map(RecT)

            case _ : RecT =>
                Some(typT)

            case Sel(r, FieldUse(f)) =>
                selectionType(r, f)

            case _ : Str =>
                Some(strT)

            case Uni() =>
                Some(uniT)

            case Var(field) =>
                makeRow(Vector(field)).map(VarT)

            case _ : VarT =>
                Some(typT)

            case Vec(elems) =>
                if (elems.optExpressions.isEmpty)
                    Some(VecNilT())
                else
                    vecLubType(elems) match {
                        case IgnoreBound | NoBound => None
                        case Bound(t)              => Some(VecT(t))
                    }

            case _ : VecNilT | _ : VecT =>
                Some(typT)

            case e =>
                sys.error(s"tipe: unexpected expression $e")
        }

    def substArgTypes[T](x : String, t : Expression, a : T) = {
        val substArgType =
            rule[Expression] {
                case Idn(IdnUse(`x`)) =>
                    t
            }
        rewrite(everywhere(substArgType))(a)
    }

    def appArgTypes(app : App) : Option[Vector[ArgumentType]] =
        appType(app)._1

    def appResType(app : App) : Option[Expression] =
        appType(app)._2

    val appType : App => (Option[Vector[ArgumentType]], Option[Expression]) =
        attr {
            case app @ App(f, as) =>
                appType(app, tipe(f), as)
        }

    def appType(n : Expression, ftype : Option[Expression], as : Vector[Expression]) : (Option[Vector[ArgumentType]], Option[Expression]) =
        ftype match {
            case Some(FunT(ArgumentTypes(ts), t)) =>
                appType(n, ts, Vector(), t, as)
            case _ =>
                (None, None)
        }

    def appType(n : Expression, ts : Vector[ArgumentType], newts : Vector[ArgumentType], t : Expression,
        as : Vector[Expression]) : (Option[Vector[ArgumentType]], Option[Expression]) =
        if (as.isEmpty)
            if (ts.isEmpty)
                (Some(newts), unalias(n, t))
            else
                (Some(newts), unaliasFunT(n, ts, t))
        else if (ts.isEmpty)
            (None, None)
        else
            ts.head match {
                case ArgumentType(Some(IdnDef(x)), TypT()) =>
                    appType(n, substArgTypes(x, as.head, ts.tail), newts :+ ts.head, substArgTypes(x, as.head, t), as.tail)
                case _ =>
                    appType(n, ts.tail, newts :+ ts.head, t, as.tail)
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
            case ArgumentEntity(n @ Argument(_, t, _)) =>
                unalias(n, t)
            case CaseValueEntity(tree.parent.pair(Case(x, _, _), Mat(e, _))) =>
                tipe(e) match {
                    case Some(VarT(r)) =>
                        r.find(_.identifier == x) match {
                            case Some(field) =>
                                unalias(e, field.expression)
                            case None =>
                                None
                        }
                    case _ =>
                        None
                }
            case FunctionEntity(Def(_, Body(Arguments(as), t, e))) =>
                unaliasFunT(e, argsToArgTypes(as), t)
            case LetEntity(Let(_, i, None, e)) =>
                tipe(e)
            case LetEntity(Let(_, _, Some(LetType(tipe)), e)) =>
                if (tree.nodes.contains(e))
                    unalias(e, tipe)
                else
                    Some(tipe)
            case PredefLetEntity(_, tipe, _) =>
                Some(tipe)
            case PredefTypedEntity(_, tipe) =>
                Some(tipe)
            case _ =>
                None
        }

    def selectionType(r : Expression, f : String) : Option[Expression] =
        tipe(r) match {
            case Some(RecT(fieldTypes)) =>
                fieldType(fieldTypes, f)
            case _ =>
                None
        }

    def fieldType(fieldTypes : Vector[FieldType], f : String) : Option[Expression] =
        fieldTypes.find {
            case FieldType(i, t) =>
                i == f
        } match {
            case Some(FieldType(i, t)) =>
                Some(t)
            case None =>
                None
        }

    sealed abstract class MatchType
    case class BadCases() extends MatchType
    case class OkCases(optType : Option[Expression]) extends MatchType

    lazy val matchType : Mat => BoundResult =
        attr { case Mat(_, cases) => getLub(cases.map(_.expression)) }

    val unaliasedType : Expression => Option[Expression] =
        attr {
            case e =>
                unalias(e, tipe(e))
        }

    def unalias(n : ASTNode, optType : Option[Expression]) : Option[Expression] =
        optType.flatMap(t => unalias(n, t))

    def unalias(n : ASTNode, t : Expression) : Option[Expression] =
        t match {
            case n @ App(Idn(f), as) =>
                appResType(n) match {
                    case Some(t) if t == typT =>
                        val optArgsAndBody =
                            entity(f) match {
                                case FunctionEntity(Def(_, Body(Arguments(das), _, body))) =>
                                    Some((das, body))
                                case LetEntity(Let(_, _, _, Fun(Arguments(das), body))) =>
                                    Some((das, body))
                                case PredefLetEntity(_, _, Fun(Arguments(das), body)) =>
                                    Some((das, body))
                                case _ =>
                                    None
                            }
                        optArgsAndBody.flatMap {
                            case (das, body) =>
                                appType(n, argsToArgTypes(das), Vector(), body, as)._2
                        }
                    case _ =>
                        None
                }

            case Cat(l, r) =>
                (unalias(n, l), unalias(n, r)) match {
                    case (Some(RecT(l)), Some(RecT(r))) =>
                        Some(RecT(l ++ r))
                    case _ =>
                        None
                }

            case PrimitiveType() =>
                Some(t)

            case Idn(IdnUse(x)) =>
                lookup(env(n), x, UnknownEntity()) match {
                    case ArgumentEntity(Argument(_, TypT(), _)) =>
                        Some(t)
                    case LetEntity(Let(_, _, _, v)) if t != v =>
                        unalias(n, v)
                    case PredefLetEntity(_, TypT(), v) =>
                        unalias(n, v)
                    case e =>
                        None
                }

            case FunT(ArgumentTypes(us), u) =>
                unaliasFunT(n, us, u)

            case RecT(fieldTypes) =>
                unaliasRecT(n, fieldTypes)

            case VarT(fieldTypes) =>
                unaliasVarT(n, fieldTypes)

            case VecT(t) =>
                unaliasVecT(n, t)

            case _ =>
                Some(t)
        }

    def unaliases(n : ASTNode, ts : Vector[Expression]) : Option[Vector[Expression]] = {
        val us = ts.map(t => unalias(n, t))
        if (us.forall(_.isDefined))
            Some(us.map(_.get))
        else
            None
    }

    def unaliasArg(n : ASTNode, a : ArgumentType) : Option[ArgumentType] =
        unalias(n, a.expression) match {
            case Some(e) =>
                Some(ArgumentType(a.optIdnDef, e))
            case None =>
                None
        }

    def unaliasArgs(n : ASTNode, as : Vector[ArgumentType]) : Option[Vector[ArgumentType]] = {
        val us = as.map(t => unaliasArg(n, t))
        if (us.forall(_.isDefined))
            Some(us.map(_.get))
        else
            None
    }

    def unaliasFunT(n : ASTNode, as : Vector[ArgumentType], t : Expression) : Option[FunT] =
        unaliasArgs(n, as) match {
            case Some(us) =>
                unalias(n, t) match {
                    case Some(u) =>
                        Some(FunT(ArgumentTypes(us), u))
                    case None =>
                        None
                }
            case None =>
                None
        }

    def unaliasFieldTypes(n : ASTNode, fts : Vector[FieldType]) : Option[Vector[FieldType]] = {
        val is = fts.map(_.identifier)
        val ts = fts.map(_.expression)
        unaliases(n, ts) match {
            case Some(us) =>
                Some(is.zip(us).map {
                    case (i, t) =>
                        FieldType(i, t)
                })
            case None =>
                None
        }
    }

    def unaliasRecT(n : ASTNode, fts : Vector[FieldType]) : Option[RecT] =
        unaliasFieldTypes(n, fts).map(RecT)

    def unaliasVarT(n : ASTNode, fts : Vector[FieldType]) : Option[VarT] =
        unaliasFieldTypes(n, fts).map(VarT)

    def unaliasVecT(n : ASTNode, t : Expression) : Option[VecT] =
        unalias(n, t) match {
            case Some(u) => Some(VecT(u))
            case None    => None
        }

    lazy val blockTipe : BlockExp => Option[Expression] =
        attr {
            case BlkDef(_, b) => blockTipe(b)
            case BlkLet(_, b) => blockTipe(b)
            case Return(e)    => tipe(e)
        }

    lazy val expectedType : Expression => Option[Expression] =
        attr {
            case tree.parent.pair(a, app @ App(e, _)) if a ne e =>
                val argnum = tree.index(a) - 1
                appArgTypes(app) match {
                    case Some(ts) if ts.length >= argnum =>
                        Some(ts(argnum).expression)
                    case _ =>
                        None
                }

            case tree.parent(_ : Argument) =>
                Some(typT)

            case tree.parent(_ : ArgumentType) =>
                Some(typT)

            case tree.parent.pair(a, Body(_, t, e)) =>
                if (a eq t)
                    Some(typT)
                else
                    unalias(e, t)

            case tree.parent(_ : FieldType) =>
                Some(typT)

            case tree.parent(_ : FunT) =>
                Some(typT)

            case tree.parent(_ : LetType) =>
                Some(typT)

            case tree.parent.pair(_ : Expression, Let(_, _, Some(LetType(t)), e)) =>
                unalias(e, t)

            case tree.parent.pair(_ : Expression, Let(_, _, None, e)) =>
                unalias(e, tipe(e))

            case tree.parent.pair(a : Expression, Prm(EqualP(), Vector(t, l, r))) =>
                if (a eq t)
                    Some(typT)
                else
                    unalias(t, t)

            case tree.parent.pair(a : Expression, Prm(p, as)) =>
                val argnum = tree.index(a) - 1
                userPrimitiveType(p) match {
                    case FunT(ArgumentTypes(ts), _) if (as.length == ts.length) && (ts.length >= argnum) =>
                        unalias(a, ts(argnum).expression)
                    case _ =>
                        None
                }

            case tree.parent(_ : VecT) =>
                Some(typT)

            case _ =>
                None
        }

    lazy val replType : REPLInput => Option[Expression] =
        attr {
            case REPLExp(e) =>
                tipe(e)
            case REPLDef(Def(_, Body(Arguments(as), t, e))) =>
                unaliasFunT(e, argsToArgTypes(as), t)
            case REPLLet(Let(_, _, None, e)) =>
                tipe(e)
            case REPLLet(Let(_, _, Some(LetType(t)), e)) =>
                Some(t)
        }

    lazy val replTypeValue : REPLInput => Option[Expression] =
        attr {
            case REPLExp(e) =>
                unalias(e, e)
            case REPLLet(Let(_, _, None, e)) =>
                unalias(e, e)
            case REPLLet(Let(_, _, Some(LetType(t)), e)) =>
                unalias(e, e)
            case _ =>
                None
        }

    def fieldtypeNames(fts : Vector[FieldType]) : Vector[String] =
        fts.map(_.identifier)

    def subtype(ot : Option[Expression], u : Expression) : Boolean =
        ot match {
            case Some(t) =>
                subtype(t, u)
            case None =>
                true
        }

    def subtype(t : Expression, u : Expression) : Boolean = {
        (t == u) ||
            ((unalias(t, t), unalias(u, u)) match {
                case (Some(RecT(tr)), Some(RecT(ur))) =>
                    val urn = fieldtypeNames(ur)
                    urn.diff(fieldtypeNames(tr)).isEmpty && subtypesFields(urn, tr, ur)
                case (Some(VarT(tr)), Some(VarT(ur))) =>
                    val trn = fieldtypeNames(tr)
                    trn.diff(fieldtypeNames(ur)).isEmpty && subtypesFields(trn, tr, ur)
                case (Some(FunT(ArgumentTypes(ts), t)), Some(FunT(ArgumentTypes(us), u))) =>
                    subtypesArgs(us, ts) && subtype(t, u)
                case (Some(VecNilT()), Some(VecT(_))) =>
                    true
                case (Some(VecT(t)), Some(VecT(u))) =>
                    subtype(t, u)
                case _ =>
                    false
            })
    }

    def subtypesArgs(ts : Vector[ArgumentType], us : Vector[ArgumentType]) : Boolean =
        (ts.length == us.length) &&
            (ts.zip(us).forall {
                case (t, u) =>
                    subtype(t.expression, u.expression)
            })

    def fieldTypesToMap(ts : Vector[FieldType]) : Map[String, Expression] =
        ts.map {
            case FieldType(x, t) =>
                (x, t)
        }.toMap

    def subtypesFields(ns : Vector[String], ts : Vector[FieldType], us : Vector[FieldType]) : Boolean = {
        val tsm = fieldTypesToMap(ts)
        val usm = fieldTypesToMap(us)
        ns.forall(x => subtype(tsm(x), usm(x)))
    }

    def subtypes(ts : Vector[Expression], us : Vector[Expression]) : Boolean =
        (ts.length == us.length) &&
            (ts.zip(us).forall {
                case (t, u) =>
                    subtype(t, u)
            })

    sealed trait BoundResult extends Product
    case object IgnoreBound extends BoundResult
    case object NoBound extends BoundResult
    case class Bound(tipe : Expression) extends BoundResult

    sealed trait BoundDirection extends Product {
        def bound =
            this match {
                case Lub => lub _
                case Glb => glb _
            }
    }

    case object Lub extends BoundDirection
    case object Glb extends BoundDirection

    def getLub(es : Vector[Expression]) : BoundResult = {
        val tipesOpt = es.map(tipe)
        // unwrap options, if `None` appears then ignore bound (error occurred elsewhere)
        @tailrec
        def flatten(
            tipesOpt : Vector[Option[Expression]],
            out : Vector[Expression]
        ) : Option[Vector[Expression]] =
            tipesOpt match {
                case Some(hd) +: tl => flatten(tl, out :+ hd)
                case None +: _      => None
                case _              => Some(out)
            }
        flatten(tipesOpt, Vector.empty) match {
            case Some(hd +: tl) =>
                @tailrec
                def aux(
                    tipes : Vector[Expression],
                    out : Expression
                ) : BoundResult =
                    (tipes, out) match {
                        case (u +: tl, t) =>
                            lub(t, u) match {
                                case Some(t) => aux(tl, t)
                                case None    => NoBound
                            }
                        case (_, t) =>
                            Bound(t)
                    }
                aux(tl, hd)
            case _ =>
                // empty or error occurred elsewhere
                IgnoreBound
        }
    }

    // for covariant records and contravariant variants
    def rowLub(
        tr : Vector[FieldType],
        ur : Vector[FieldType],
        direction : BoundDirection
    ) : Option[Vector[FieldType]] = {
        // ordering of fields: according to `t`
        val urMap = ur.map { case FieldType(idn, tipe) => idn -> tipe }.toMap
        @tailrec
        def aux(
            tr : Vector[FieldType],
            out : Vector[FieldType]
        ) : Option[Vector[FieldType]] = {
            tr match {
                case FieldType(idn, tt) +: tl =>
                    urMap.get(idn) match {
                        case Some(ut) =>
                            // common field, find bound
                            direction.bound(tt, ut) match {
                                case Some(t) =>
                                    // found bound
                                    aux(tl, out :+ FieldType(idn, t))
                                case None =>
                                    // no bound
                                    None
                            }
                        case None =>
                            // field not in `u`, omit
                            aux(tl, out)
                    }
                case _ =>
                    if (out.isEmpty) None
                    else Some(out)
            }
        }
        aux(tr, Vector.empty)
    }

    // for contravariant records and covariant variants
    def rowGlb(
        tr : Vector[FieldType],
        ur : Vector[FieldType],
        direction : BoundDirection
    ) : Option[Vector[FieldType]] = {
        // ordering of fields: all of `t`, then all of `u` minus `t`
        val urMap = ur.map { case FieldType(idn, tipe) => idn -> tipe }.toMap
        @tailrec
        def aux(
            tr : Vector[FieldType],
            out : SeqMap[String, Expression]
        ) : Option[Vector[FieldType]] =
            tr match {
                case FieldType(idn, tt) +: tl =>
                    urMap.get(idn) match {
                        case Some(ut) =>
                            // common field, find bound
                            direction.bound(tt, ut) match {
                                case Some(t) =>
                                    // found bound
                                    aux(tl, out + (idn -> t))
                                case None =>
                                    // no bound, abort
                                    None
                            }
                        case None =>
                            // field not in `u`, retain as in `t`
                            aux(tl, out + (idn -> tt))
                    }
                case _ =>
                    // include all fields in `u` minus `t`
                    val trx = out.toVector.map { case (idn, t) => FieldType(idn, t) }
                    val urx = ur.filterNot { case FieldType(idn, _) => out.contains(idn) }
                    val result = trx ++ urx
                    if (result.isEmpty) None
                    else Some(result)
            }
        aux(tr, SeqMap.empty)
    }

    def funBound(t : FunT, u : FunT, direction : BoundDirection) : Option[FunT] = {
        val (FunT(ArgumentTypes(tats), trt), FunT(ArgumentTypes(uats), urt)) = (t, u)
        if (tats.length != uats.length)
            // arity mismatch
            None
        else {
            // covariance and contravariance
            val (checkArg, checkReturn) =
                direction match {
                    // lub(A0 -> B0, A1 -> B1) = glb(A0, A1) -> lub(B0, B1)
                    case Lub => (glb _, lub _)
                    // glb(A0 -> B0, A1 -> B1) = lub(A0, A1) -> glb(B0, B1)
                    case Glb => (lub _, glb _)
                }
            // check return type
            checkReturn(trt, urt) match {
                case Some(rt) =>
                    // check arguments
                    @tailrec
                    def aux(
                        ats : Vector[(ArgumentType, ArgumentType)],
                        out : Vector[ArgumentType]
                    ) : Option[Vector[ArgumentType]] =
                        ats match {
                            case (ArgumentType(tio, tt), ArgumentType(uio, ut)) +: tl =>
                                // prefer identifier name on the left
                                val idnOpt =
                                    (tio, uio) match {
                                        case (Some(ti), _)    => Some(ti)
                                        case (None, Some(ui)) => Some(ui)
                                        case (None, None)     => None
                                    }
                                checkArg(tt, ut) match {
                                    case Some(t) =>
                                        // found bound, continue
                                        aux(tl, out :+ ArgumentType(idnOpt, t))
                                    case None =>
                                        // no bound, abort
                                        None
                                }
                            case _ =>
                                Some(out)
                        }
                    aux(tats.zip(uats), Vector.empty).map(ats => FunT(ArgumentTypes(ats), rt))
                case None =>
                    None
            }
        }
    }

    def lub(t : Expression, u : Expression) : Option[Expression] = {
        val unaliased =
            (unalias(t, t), unalias(u, u)) match {
                case (Some(t), Some(u)) => Some((t, u))
                case _                  => None
            }
        unaliased.flatMap {
            // type variables
            case (Idn(IdnUse(ti)), Idn(IdnUse(ui))) =>
                if (ti == ui) Some(Idn(IdnUse(ti)))
                else None
            // unit
            case (UniT(), UniT()) =>
                Some(Idn(IdnUse("Unit")))
            // records
            case (RecT(tr), RecT(ur)) =>
                rowLub(tr, ur, Lub).map(RecT)
            // variants
            case (VarT(tr), VarT(ur)) =>
                rowGlb(tr, ur, Lub).map(VarT)
            // vectors
            case (VecT(tt), VecT(ut)) =>
                lub(tt, ut).map(VecT)
            case (VecNilT(), VecT(t)) =>
                Some(VecT(t))
            case (VecT(t), VecNilT()) =>
                Some(VecT(t))
            case (VecNilT(), VecNilT()) =>
                Some(VecNilT())
            // functions
            case (t : FunT, u : FunT) =>
                funBound(t, u, Lub)
            // atomics
            case (StrT(), StrT()) =>
                Some(Idn(IdnUse("String")))
            case (IntT(), IntT()) =>
                Some(Idn(IdnUse("Int")))
            case _ =>
                None
        }
    }

    def glb(t : Expression, u : Expression) : Option[Expression] = {
        val unaliased =
            (unalias(t, t), unalias(u, u)) match {
                case (Some(t), Some(u)) => Some((t, u))
                case _                  => None
            }
        unaliased.flatMap {
            // type variables
            case (Idn(IdnUse(ti)), Idn(IdnUse(ui))) =>
                if (ti == ui) Some(Idn(IdnUse(ti)))
                else None
            // unit
            case (UniT(), UniT()) =>
                Some(Idn(IdnUse("Unit")))
            // records
            case (RecT(tr), RecT(ur)) =>
                rowGlb(tr, ur, Glb).map(RecT)
            // variants
            case (VarT(tr), VarT(ur)) =>
                rowLub(tr, ur, Glb).map(VarT)
            // vectors
            case (VecT(tt), VecT(ut)) =>
                glb(tt, ut).map(VecT)
            case (VecNilT(), _) | (VecNilT(), _) =>
                Some(VecNilT())
            // functions
            case (t : FunT, u : FunT) =>
                funBound(t, u, Glb)
            // atomics
            case (StrT(), StrT()) =>
                Some(Idn(IdnUse("String")))
            case (IntT(), IntT()) =>
                Some(Idn(IdnUse("Int")))
            case _ =>
                None
        }
    }

}
