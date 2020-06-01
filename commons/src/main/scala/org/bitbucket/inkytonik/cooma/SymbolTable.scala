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

    // Secret boolean
    val secBoolT : Expression =
        VarT(Vector(FieldType("False", SecT(UniT())), FieldType("True", SecT(UniT()))))

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

    // Secret capabilities
    val secReaderT : Expression =
        RecT(Vector(
            FieldType("read", FunT(ArgumentTypes(Vector()), SecT(StrT())))
        ))

    val secReaderWriterT : Expression =
        RecT(Vector(
            FieldType("read", FunT(ArgumentTypes(Vector()), SecT(StrT()))),
            FieldType("write", FunT(ArgumentTypes(Vector(ArgumentType(Some(IdnDef("s")), SecT(StrT())))), SecT(UniT())))
        ))

    val secWriterT : Expression =
        RecT(Vector(
            FieldType("write", FunT(ArgumentTypes(Vector(ArgumentType(Some(IdnDef("s")), SecT(StrT())))), SecT(UniT())))
        ))

    def mkPrimType(args : Vector[Expression], retType : Expression) : FunT =
        FunT(ArgumentTypes(args.map { case e => ArgumentType(None, e) }), retType)

    def mkPrimTypeWithArgNames(args : Vector[(String, Expression)], retType : Expression) : FunT =
        FunT(ArgumentTypes(args.map { case (x, e) => ArgumentType(Some(IdnDef(x)), e) }), retType)

    def mkIntUnPrimType(retType : Expression, secret : Boolean) : FunT =
        if (secret) {
            mkPrimType(Vector(SecT(IntT())), retType)
        } else {
            mkPrimType(Vector(IntT()), retType)
        }

    def mkIntBinPrimType(retType : Expression, secret : Boolean) : FunT =
        if (secret) {
            mkPrimType(Vector(SecT(IntT()), SecT(IntT())), retType)
        } else {
            mkPrimType(Vector(IntT(), IntT()), retType)
        }

    val primitivesTypesTable = Map(
        "Equal" -> mkPrimTypeWithArgNames(Vector(("t", TypT()), ("l", Idn(IdnUse("t"))), ("l", Idn(IdnUse("t")))), boolT),
        "IntAbs" -> mkIntUnPrimType(IntT(), false),
        "IntAdd" -> mkIntBinPrimType(IntT(), false),
        "IntSub" -> mkIntBinPrimType(IntT(), false),
        "IntMul" -> mkIntBinPrimType(IntT(), false),
        "IntDiv" -> mkIntBinPrimType(IntT(), false),
        "IntPow" -> mkIntBinPrimType(IntT(), false),
        "IntGt" -> mkIntBinPrimType(boolT, false),
        "IntGte" -> mkIntBinPrimType(boolT, false),
        "IntLt" -> mkIntBinPrimType(boolT, false),
        "IntLte" -> mkIntBinPrimType(boolT, false),
        "StrConcat" -> mkPrimType(Vector(StrT(), StrT()), StrT()),
        "StrLength" -> mkPrimType(Vector(StrT()), IntT()),
        "StrSubstr" -> mkPrimType(Vector(StrT(), IntT()), StrT()),
        // Secret versions
        "EqualS" -> mkPrimTypeWithArgNames(Vector(("t", TypT()), ("l", Idn(IdnUse("t"))), ("l", Idn(IdnUse("t")))), secBoolT),
        "IntAbsS" -> mkIntUnPrimType(SecT(IntT()), true),
        "IntAddS" -> mkIntBinPrimType(SecT(IntT()), true),
        "IntSubS" -> mkIntBinPrimType(SecT(IntT()), true),
        "IntMulS" -> mkIntBinPrimType(SecT(IntT()), true),
        "IntDivS" -> mkIntBinPrimType(SecT(IntT()), true),
        "IntPowS" -> mkIntBinPrimType(SecT(IntT()), true),
        "IntGtS" -> mkIntBinPrimType(secBoolT, true),
        "IntGteS" -> mkIntBinPrimType(secBoolT, true),
        "IntLtS" -> mkIntBinPrimType(secBoolT, true),
        "IntLteS" -> mkIntBinPrimType(secBoolT, true),
        "StrConcatS" -> mkPrimType(Vector(SecT(StrT()), SecT(StrT())), SecT(StrT())),
        "StrLengthS" -> mkPrimType(Vector(SecT(StrT())), SecT(IntT())),
        "StrSubstrS" -> mkPrimType(Vector(SecT(StrT()), SecT(IntT())), SecT(StrT()))
    )

}
