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

    // Short-hands for primitive types

    val intT : Expression = Idn(IdnUse("Int"))
    val strT : Expression = Idn(IdnUse("String"))
    val typT : Expression = Idn(IdnUse("Type"))
    val uniT : Expression = Idn(IdnUse("Unit"))

    def isPrimitiveTypeName(s : String) : Boolean =
        (s == "Int") | (s == "String") | (s == "Type") | (s == "Unit")

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

    val boolT : Expression =
        VarT(Vector(FieldType("False", uniT), FieldType("True", uniT)))

    private def httpT(method : String) : Expression =
        RecT(Vector(FieldType(method, FunT(
            ArgumentTypes(Vector(ArgumentType(Some(IdnDef("suffix")), strT))),
            RecT(Vector(
                FieldType("code", intT),
                FieldType("body", strT)
            ))
        ))))

    val httpDeleteT : Expression = httpT("delete")
    val httpGetT : Expression = httpT("get")
    val httpPostT : Expression = httpT("post")
    val httpPutT : Expression = httpT("put")

    val readerT : Expression =
        RecT(Vector(
            FieldType("read", FunT(ArgumentTypes(Vector()), strT))
        ))

    val writerT : Expression =
        RecT(Vector(
            FieldType("write", FunT(ArgumentTypes(Vector(ArgumentType(Some(IdnDef("s")), strT))), uniT))
        ))

    val folderReaderT : Expression =
        RecT(Vector(
            FieldType("read", FunT(ArgumentTypes(Vector(ArgumentType(Some(IdnDef("suffix")), strT))), strT))
        ))

    val folderWriterT : Expression =
        RecT(Vector(
            FieldType("write", FunT(ArgumentTypes(Vector(ArgumentType(Some(IdnDef("suffix")), strT), ArgumentType(Some(IdnDef("text")), strT))), uniT))
        ))

    def mkPrimType(args : Vector[Expression], retType : Expression) : FunT =
        FunT(ArgumentTypes(args.map { case e => ArgumentType(None, e) }), retType)

    def mkPrimTypeWithArgNames(args : Vector[(String, Expression)], retType : Expression) : FunT =
        FunT(ArgumentTypes(args.map { case (x, e) => ArgumentType(Some(IdnDef(x)), e) }), retType)

    def mkIntUnPrimType(retType : Expression) : FunT =
        mkPrimType(Vector(intT), retType)

    def mkIntBinPrimType(retType : Expression) : FunT =
        mkPrimType(Vector(intT, intT), retType)

    val primitivesTypesTable = Map(
        "Equal" -> mkPrimTypeWithArgNames(Vector(("t", typT), ("l", Idn(IdnUse("t"))), ("l", Idn(IdnUse("t")))), boolT),
        "IntAbs" -> mkIntUnPrimType(intT),
        "IntAdd" -> mkIntBinPrimType(intT),
        "IntSub" -> mkIntBinPrimType(intT),
        "IntMul" -> mkIntBinPrimType(intT),
        "IntDiv" -> mkIntBinPrimType(intT),
        "IntPow" -> mkIntBinPrimType(intT),
        "IntGt" -> mkIntBinPrimType(boolT),
        "IntGte" -> mkIntBinPrimType(boolT),
        "IntLt" -> mkIntBinPrimType(boolT),
        "IntLte" -> mkIntBinPrimType(boolT),
        "StrConcat" -> mkPrimType(Vector(strT, strT), strT),
        "StrLength" -> mkPrimType(Vector(strT), intT),
        "StrSubstr" -> mkPrimType(Vector(strT, intT), strT)
    )

}
