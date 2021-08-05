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

import org.bitbucket.inkytonik.kiama.util.{Entity, Environments}

/**
 * Superclass of all Cooma entities. Provides generic access to
 * the declaration node of the entity and a textual description.
 */
sealed abstract class CoomaEntity extends Entity with Product

/**
 * Symbol table module containing facilities for creating and
 * manipulating Cooma language symbol information.
 */
object SymbolTable extends Environments[CoomaEntity] {

    import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
    import org.bitbucket.inkytonik.kiama.util.{FileSource, Messaging, Positions}

    /**
     * A MiniJava entity that represents a legally resolved entity.
     */
    sealed abstract class CoomaOkEntity extends CoomaEntity {
        def decl : ASTNode
        def desc : String
    }

    case class ArgumentEntity(decl : Argument) extends CoomaOkEntity {
        val desc = "argument"
    }

    case class CaseValueEntity(decl : Case) extends CoomaOkEntity {
        val desc = "case value"
    }

    case class FunctionEntity(decl : Def) extends CoomaOkEntity {
        val desc = "function"
    }

    case class LetEntity(decl : Let) extends CoomaOkEntity {
        val desc =
            decl.letKind match {
                case Type() => "type"
                case Val()  => "value"
            }
    }

    case class PredefTypedEntity(name : String, tipe : Expression) extends CoomaOkEntity {
        val decl = null
        val desc = "predef typed"
    }

    case class PredefLetEntity(name : String, tipe : Expression, value : Expression) extends CoomaOkEntity {
        val decl = null
        val desc = "predef typed and valued"
    }

    /**
     * An entity represented by names for whom we have seen more than one
     * declaration so we are unsure what is being represented.
     */
    case class MultipleEntity() extends CoomaEntity {
        override val isError = true
    }

    /**
     * An unknown entity, for example one that is represened by names whose
     * declarations are missing.
     */
    case class UnknownEntity() extends CoomaEntity {
        override val isError = true
    }

    // Short-hands for types and standard type checks

    def intT : Expression = Idn(IdnUse("Int"))
    def strT : Expression = Idn(IdnUse("String"))
    def typT : Expression = Idn(IdnUse("Type"))
    def uniT : Expression = Idn(IdnUse("Unit"))

    object IntT {
        def unapply(e : Expression) : Boolean =
            e match {
                case Idn(IdnUse("Int")) =>
                    true
                case _ =>
                    false
            }
    }

    object StrT {
        def unapply(e : Expression) : Boolean =
            e match {
                case Idn(IdnUse("String")) =>
                    true
                case _ =>
                    false
            }
    }

    object TypT {
        def unapply(e : Expression) : Boolean =
            e match {
                case Idn(IdnUse("Type")) =>
                    true
                case _ =>
                    false
            }
    }

    object UniT {
        def unapply(e : Expression) : Boolean =
            e match {
                case Idn(IdnUse("Unit")) =>
                    true
                case _ =>
                    false
            }
    }

    def isPrimitiveTypeName(s : String) : Boolean =
        (s == "Int") || (s == "String") || (s == "Type") || (s == "Unit")

    object PrimitiveType {
        def unapply(e : Expression) : Boolean =
            e match {
                case Idn(IdnUse(s)) if isPrimitiveTypeName(s) =>
                    true
                case _ =>
                    false
            }
    }

    object PrimitiveTypeName {
        def unapply(s : String) : Boolean =
            PrimitiveType.unapply(Idn(IdnUse(s)))
    }

    def boolT : Expression =
        Idn(IdnUse("Boolean"))

    def isCapabilityTypeName(s : String) : Boolean =
        (s == "FolderReader") || (s == "FolderWriter") || (s == "Reader") ||
            (s == "Writer") || isHttpMethodName(s) || (s == "Table")

    def isHttpMethodName(s : String) : Boolean =
        (s == "HttpDelete") || (s == "HttpGet") || (s == "HttpPost") ||
            (s == "HttpPut")

    // Primitive types

    def mkPrimType(args : Vector[Expression], retType : Expression) : FunT =
        FunT(ArgumentTypes(args.map { case e => ArgumentType(None, e) }), retType)

    def mkPrimTypeWithArgNames(args : Vector[(String, Expression)], retType : Expression) : FunT =
        FunT(ArgumentTypes(args.map { case (x, e) => ArgumentType(Some(IdnDef(x)), e) }), retType)

    def mkVectorPrimTypeWithArgNames(args : Vector[(String, Expression)], retType : Expression) : FunT =
        mkPrimTypeWithArgNames(Vector(("t", typT), ("v", VecT(Idn(IdnUse("t"))))) ++ args, retType)

    def mkIntUnPrimType(retType : Expression) : FunT =
        mkPrimType(Vector(intT), retType)

    def mkIntBinPrimType(retType : Expression) : FunT =
        mkPrimType(Vector(intT, intT), retType)

    def userPrimitiveType(p : UserPrimitive) : FunT =
        p match {
            case EqualP() =>
                mkPrimTypeWithArgNames(
                    Vector(
                        ("t", typT),
                        ("l", Idn(IdnUse("t"))),
                        ("l", Idn(IdnUse("t")))
                    ),
                    boolT
                )
            case IntAbsP() =>
                mkPrimType(Vector(intT), intT)
            case IntAddP() | IntDivP() | IntModP() | IntMulP() | IntPowP() | IntSubP() =>
                mkPrimType(Vector(intT, intT), intT)
            case IntGtP() | IntGteP() | IntLtP() | IntLteP() =>
                mkPrimType(Vector(intT, intT), boolT)
            case StrConcatP() =>
                mkPrimType(Vector(strT, strT), strT)
            case StrLengthP() =>
                mkPrimType(Vector(strT), intT)
            case StrSubstrP() =>
                mkPrimType(Vector(strT, intT), strT)
            case VecAppendP() =>
                mkVectorPrimTypeWithArgNames(Vector(("e", Idn(IdnUse("t")))), VecT(Idn(IdnUse("t"))))
            case VecConcatP() =>
                mkVectorPrimTypeWithArgNames(Vector(("vr", VecT(Idn(IdnUse("t"))))), VecT(Idn(IdnUse("t"))))
            case VecGetP() =>
                mkVectorPrimTypeWithArgNames(Vector(("i", intT)), Idn(IdnUse("t")))
            case VecLengthP() =>
                mkVectorPrimTypeWithArgNames(Vector(), intT)
            case VecPrependP() =>
                mkVectorPrimTypeWithArgNames(Vector(("e", Idn(IdnUse("t")))), VecT(Idn(IdnUse("t"))))
            case VecPutP() =>
                mkVectorPrimTypeWithArgNames(Vector(("i", intT), ("e", Idn(IdnUse("t")))), VecT(Idn(IdnUse("t"))))
        }

    // Prelude

    def preludeStaticEnv(config : Config) : Environment =
        if (config.noPrelude() || config.compilePrelude())
            rootenv()
        else {
            val staticPreludePath = s"${config.preludePath()}.static"
            val source = FileSource(staticPreludePath)
            val positions = new Positions()
            val p = new CoomaParser(source, positions)
            val pr = p.pStaticPrelude(0)
            if (pr.hasValue) {
                val prelude = p.value(pr).asInstanceOf[StaticPrelude]
                val entries = prelude.optStaticPreludeEntrys
                entries.foldLeft(rootenv()) {
                    case (env, StaticTypedEntry(id, tipe)) =>
                        define(env, id, PredefTypedEntity(id, tipe))
                    case (env, StaticLetEntry(id, tipe, exp)) =>
                        define(env, id, PredefLetEntity(id, tipe, exp))
                }
            } else {
                val messaging = new Messaging(positions)
                config.output().emitln(s"cooma: can't read static prelude '$staticPreludePath'")
                val message = p.errorToMessage(pr.parseError)
                messaging.report(source, Vector(message), config.output())
                sys.exit(1)
            }
        }

}
