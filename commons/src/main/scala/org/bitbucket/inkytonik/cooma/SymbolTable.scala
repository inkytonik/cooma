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

    case class FieldEntity(decl : Field) extends CoomaOkEntity {
        val desc = "field"
    }

    case class FunctionEntity(decl : Def) extends CoomaOkEntity {
        val desc = "function"
    }

    case class LetEntity(decl : Let) extends CoomaOkEntity {
        val desc = "let"
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

    val boolT : Expression =
        VarT(Vector(FieldType("False", UniT()), FieldType("True", UniT())))

    val readerT : Expression =
        RecT(Vector(
            FieldType("read", FunT(ArgumentTypes(Vector()), StrT()))
        ))

    val readerWriterT : Expression =
        RecT(Vector(
            FieldType("read", FunT(ArgumentTypes(Vector()), StrT())),
            FieldType("write", FunT(ArgumentTypes(Vector(ArgumentType(Some(IdnDef("s")), StrT()))), UniT()))
        ))

    val writerT : Expression =
        RecT(Vector(
            FieldType("write", FunT(ArgumentTypes(Vector(ArgumentType(Some(IdnDef("s")), StrT()))), UniT()))
        ))

    def mkPrimType(args : Vector[Expression], retType : Expression) : FunT =
        FunT(ArgumentTypes(args.map { case e => ArgumentType(None, e) }), retType)

    def mkPrimTypeWithArgNames(args : Vector[(String, Expression)], retType : Expression) : FunT =
        FunT(ArgumentTypes(args.map { case (x, e) => ArgumentType(Some(IdnDef(x)), e) }), retType)

    def mkIntUnPrimType(retType : Expression) : FunT =
        mkPrimType(Vector(IntT()), retType)

    def mkIntBinPrimType(retType : Expression) : FunT =
        mkPrimType(Vector(IntT(), IntT()), retType)

    val primitivesTypesTable = Map(
        "Equal" -> mkPrimTypeWithArgNames(Vector(("t", TypT()), ("l", Idn(IdnUse("t"))), ("l", Idn(IdnUse("t")))), boolT),
        "IntAbs" -> mkIntUnPrimType(IntT()),
        "IntAdd" -> mkIntBinPrimType(IntT()),
        "IntSub" -> mkIntBinPrimType(IntT()),
        "IntMul" -> mkIntBinPrimType(IntT()),
        "IntDiv" -> mkIntBinPrimType(IntT()),
        "IntPow" -> mkIntBinPrimType(IntT()),
        "IntGt" -> mkIntBinPrimType(boolT),
        "IntGte" -> mkIntBinPrimType(boolT),
        "IntLt" -> mkIntBinPrimType(boolT),
        "IntLte" -> mkIntBinPrimType(boolT),
        "StrConcat" -> mkPrimType(Vector(StrT(), StrT()), StrT()),
        "StrLength" -> mkPrimType(Vector(StrT()), IntT()),
        "StrSubstr" -> mkPrimType(Vector(StrT(), IntT()), StrT()),
        "SelectItemVector" -> FunT(ArgumentTypes(
            Vector(
                ArgumentType(Some(IdnDef("t")), TypT()),
                ArgumentType(Some(IdnDef("v")), VecT(Some(Idn(IdnUse("t"))))),
                ArgumentType(Some(IdnDef("i")), IntT()),
            )
        ), Idn(IdnUse("t"))),
        "AppendItemVector" -> FunT(ArgumentTypes(
            Vector(
                ArgumentType(Some(IdnDef("t")), TypT()),
                ArgumentType(Some(IdnDef("v")), VecT(Some(Idn(IdnUse("t"))))),
                ArgumentType(Some(IdnDef("e")), Idn(IdnUse("t"))),
            )
        ), UniT()),
    )

}
