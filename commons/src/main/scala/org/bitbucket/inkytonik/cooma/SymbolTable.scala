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

    val primitivesTypesTable = Map(
        "IntAbs" -> FunT(Vector(IntT()), IntT()),
        "IntAdd" -> FunT(Vector(IntT(), IntT()), IntT()),
        "IntSub" -> FunT(Vector(IntT(), IntT()), IntT()),
        "IntMul" -> FunT(Vector(IntT(), IntT()), IntT()),
        "IntDiv" -> FunT(Vector(IntT(), IntT()), IntT()),
        "IntPow" -> FunT(Vector(IntT(), IntT()), IntT()),
        "IntEq" -> FunT(Vector(IntT(), IntT()), BoolT()),
        "IntNeq" -> FunT(Vector(IntT(), IntT()), BoolT()),
        "IntGt" -> FunT(Vector(IntT(), IntT()), BoolT()),
        "IntGte" -> FunT(Vector(IntT(), IntT()), BoolT()),
        "IntLt" -> FunT(Vector(IntT(), IntT()), BoolT()),
        "IntLte" -> FunT(Vector(IntT(), IntT()), BoolT()),
        "StrLength" -> FunT(Vector(StrT()), IntT()),
        "StrConcat" -> FunT(Vector(StrT(), StrT()), StrT()),
        "StrEquals" -> FunT(Vector(StrT(), StrT()), BoolT()),
        "StrSubstr" -> FunT(Vector(StrT(), IntT()), StrT())
    )

}
