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

    case class FieldEntity(decl : Field) extends CoomaOkEntity {
        val desc = "field"
    }

    case class FunctionEntity(decl : Def) extends CoomaOkEntity {
        val desc = "function"
    }

    case class TypeEntity(decl : Typ) extends CoomaOkEntity {
        val desc = "type"
    }

    case class ValueEntity(decl : Val) extends CoomaOkEntity {
        val desc = "value"
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

    //  Declarations for the pre-defined entities

    val unit = Rec(Vector())
    val unitType = PiT(RowType(Vector()))

    def makeRecordTypeEntity(
        name : String,
        fields : Vector[(String, Type)]
    ) : TypeEntity = {
        val fieldTypes = fields.map { case (n, t) => FieldType(n, t) }
        TypeEntity(Typ(IdnDef(name), PiT(RowType(fieldTypes))))
    }

    val readType = FunT(Vector(), StrT())
    val writeType = FunT(Vector(StrT()), unitType)

    val readerEntity =
        makeRecordTypeEntity(
            "Reader",
            Vector(
                ("read", readType)
            )
        )

    val writerEntity =
        makeRecordTypeEntity(
            "Writer",
            Vector(
                ("write", writeType)
            )
        )

    val readerWriterEntity =
        makeRecordTypeEntity(
            "Writer",
            Vector(
                ("read", readType),
                ("write", writeType)
            )
        )
    /**
     * Bindings for the pre-defined entities.
     */
    val predefBindings =
        Vector(
            ("Reader", readerEntity),
            ("Writer", writerEntity),
            ("ReaderWriter", readerWriterEntity)
        )

    /**
     * Pre-defined environment. Program environments extend this.
     */
    val predef : Environment =
        rootenv(predefBindings : _*)

}
