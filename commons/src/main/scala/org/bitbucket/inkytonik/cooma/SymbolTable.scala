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
    import org.bitbucket.inkytonik.kiama.relation.Tree
    import org.bitbucket.inkytonik.kiama.util.{Messaging, Positions, StringSource}

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


    val primitivesTypesTable = Map(
        "IntAdd" -> FunT(Vector(IntT(), IntT()), IntT()),
        "IntSub" -> FunT(Vector(IntT(), IntT()), IntT()),
        "IntMul" -> FunT(Vector(IntT(), IntT()), IntT()),
        "IntDiv" -> FunT(Vector(IntT(), IntT()), IntT()),
        "IntPow" -> FunT(Vector(IntT(), IntT()), IntT()),
        //"IntLTE" -> FunT(Vector(IntT(), IntT()), //get the Boolean type from the predef environment ),
        "StrConcat" -> FunT(Vector(StrT(), StrT()), StrT()),
    )


    // Pre-defined entities

    val predefSource =
        new StringSource("""
            {
                // Boolean
                val Boolean = <false : Unit, true : Unit>
                val false = <false = {}>
                val true = <true = {}>

                // Capability types
                val Reader = {read : () => String}
                val ReaderWriter = {read : () => String, write : (String) => Unit}
                val Writer = {write : (String) => Unit}

                // Primitives
                val Ints = {
                    add = fun (x : Int, y : Int) = prim IntAdd(x, y),
					sub = fun (x : Int, y : Int) = prim IntSub(x, y),
                    mul = fun (x : Int, y : Int) = prim IntMul(x, y),
	                div = fun (x : Int, y : Int) = prim IntDiv(x, y),
                    pow = fun (x : Int, y : Int) = prim IntPow(x, y)
                }

                0
            }
        """)

    /**
     * Pre-defined environment. Program environments extend this.
     * It's created by processing the predef source in an empty
     * environment and extracting the final environment.
     */
    val predef : Environment = {
        val positions = new Positions()
        val messaging = new Messaging(positions)
        val p = new CoomaParser(predefSource, positions)
        val pr = p.pProgram(0)
        if (pr.hasValue) {
            val program = p.value(pr).asInstanceOf[Program]
            val tree = new Tree[ASTNode, Program](program)
            val analyser = new SemanticAnalyser(tree, rootenv())
            analyser.errors match {
                case Vector() =>
                    analyser.deepEnv(program.expression)
                case messages =>
                    println("Cooma: processing program found errors:\n")
                    println(messaging.formatMessages(messages))
                    sys.exit(1)
            }
        } else {
            val message = p.errorToMessage(pr.parseError)
            println("Cooma: can't parse predef:\n")
            println(messaging.formatMessage(message))
            sys.exit(1)
        }
    }



}
