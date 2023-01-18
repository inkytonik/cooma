/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019-2023 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.cooma

import org.bitbucket.inkytonik.kiama.util.{
  Entity,
  Environments,
  Message,
  Source
}
import java.io.File
import org.bitbucket.inkytonik.kiama.util.StringSource
import java.io.BufferedReader
import java.io.InputStreamReader
import java.util.stream.Collectors

/** Superclass of all Cooma entities. Provides generic access to the declaration
  * node of the entity and a textual description.
  */
sealed abstract class CoomaEntity extends Entity with Product

/** Symbol table module containing facilities for creating and manipulating
  * Cooma language symbol information.
  */
object SymbolTable extends Environments[CoomaEntity] {

  import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
  import org.bitbucket.inkytonik.kiama.util.{FileSource, Messaging, Positions}

  /** A MiniJava entity that represents a legally resolved entity.
    */
  sealed abstract class CoomaOkEntity extends CoomaEntity {
    def decl: ASTNode
    def desc: String
  }

  case class ArgumentEntity(decl: Argument) extends CoomaOkEntity {
    val desc = "argument"
  }

  case class CaseValueEntity(decl: Case) extends CoomaOkEntity {
    val desc = "case value"
  }

  case class FunctionEntity(decl: Def) extends CoomaOkEntity {
    val desc = "function"
  }

  case class LetEntity(decl: Let) extends CoomaOkEntity {
    val desc =
      decl match {
        case _: TypeLet => "type"
        case _: ValLet  => "value"
      }

    val idnDef =
      decl match {
        case TypeLet(idnDef, _)   => idnDef
        case ValLet(idnDef, _, _) => idnDef
      }

    val id = idnDef.identifier
  }

  case class PredefTypedEntity(name: String, tipe: Type) extends CoomaOkEntity {
    val decl = null
    val desc = "predef typed"
  }

  case class PredefValEntity(name: String, tipe: Type, value: Expression)
      extends CoomaOkEntity {
    val decl = null
    val desc = "predef val"
  }

  case class PredefTypeEntity(name: String, tipe: Type) extends CoomaOkEntity {
    val decl = null
    val desc = "predef type"
  }

  /** An entity represented by names for whom we have seen more than one
    * declaration so we are unsure what is being represented.
    */
  case class MultipleEntity() extends CoomaEntity {
    override val isError = true
  }

  /** An unknown entity, for example one that is represened by names whose
    * declarations are missing.
    */
  case class UnknownEntity() extends CoomaEntity {
    override val isError = true
  }

  // Short-hands for types and standard type checks

  def boolT: Type = IdnT(IdnUse("Boolean"))
  def intT: Type = IdnT(IdnUse("Int"))
  def strT: Type = IdnT(IdnUse("String"))
  def typT: Type = IdnT(IdnUse("Type"))
  def uniT: Type = IdnT(IdnUse("Unit"))

  object BoolT {
    def unapply(e: Type): Boolean =
      e match {
        case IdnT(IdnUse("Boolean")) =>
          true
        case _ =>
          false
      }
  }

  object IntT {
    def unapply(e: Type): Boolean =
      e match {
        case IdnT(IdnUse("Int")) =>
          true
        case _ =>
          false
      }
  }

  object StrT {
    def unapply(e: Type): Boolean =
      e match {
        case IdnT(IdnUse("String")) =>
          true
        case _ =>
          false
      }
  }

  object TypT {
    def unapply(e: Type): Boolean =
      e match {
        case IdnT(IdnUse("Type")) =>
          true
        case _ =>
          false
      }
  }

  object UniT {
    def unapply(e: Type): Boolean =
      e match {
        case IdnT(IdnUse("Unit")) =>
          true
        case _ =>
          false
      }
  }

  def isPrimitiveTypeName(s: String): Boolean =
    (s == "Int") || (s == "String") || (s == "Type") || (s == "Unit")

  object PrimitiveType {
    def unapply(e: Type): Boolean =
      e match {
        case IdnT(IdnUse(s)) if isPrimitiveTypeName(s) =>
          true
        case _ =>
          false
      }
  }

  object PrimitiveTypeName {
    def unapply(s: String): Boolean =
      PrimitiveType.unapply(IdnT(IdnUse(s)))
  }

  val httpMethodNames =
    Set("HttpDelete", "HttpGet", "HttpPost", "HttpPut")

  val capabilityTypeNames =
    Set(
      "Database",
      "FolderReader",
      "FolderRunner",
      "FolderWriter",
      "HttpServer",
      "Reader",
      "Runner",
      "Writer"
    ) ++ httpMethodNames

  // Primitive types

  def mkPrimType(args: Vector[Type], retType: Type): FunT =
    FunT(ArgumentTypes(args.map { case e => ArgumentType(None, e) }), retType)

  def mkPrimTypeWithArgNames(
      args: Vector[(String, Type)],
      retType: Type
  ): FunT =
    FunT(
      ArgumentTypes(args.map { case (x, e) =>
        ArgumentType(Some(IdnDef(x)), e)
      }),
      retType
    )

  def mkVectorPrimTypeWithArgNames(
      args: Vector[(String, Type)],
      retType: Type
  ): FunT =
    mkPrimTypeWithArgNames(
      Vector(("t", typT), ("v", VecT(IdnT(IdnUse("t"))))) ++ args,
      retType
    )

  def mkIntUnPrimType(retType: Type): FunT =
    mkPrimType(Vector(intT), retType)

  def mkIntBinPrimType(retType: Type): FunT =
    mkPrimType(Vector(intT, intT), retType)

  def userPrimitiveType(p: UserPrimitive): FunT =
    p match {
      case EqualP() =>
        mkPrimTypeWithArgNames(
          Vector(
            ("t", typT),
            ("l", IdnT(IdnUse("t"))),
            ("l", IdnT(IdnUse("t")))
          ),
          boolT
        )
      case IntAbsP() =>
        mkPrimType(Vector(intT), intT)
      case IntAddP() | IntDivP() | IntModP() | IntMulP() | IntPowP() |
          IntSubP() =>
        mkPrimType(Vector(intT, intT), intT)
      case IntGtP() | IntGteP() | IntLtP() | IntLteP() =>
        mkPrimType(Vector(intT, intT), boolT)
      case StrConcatP() =>
        mkPrimType(Vector(strT, strT), strT)
      case StrLengthP() =>
        mkPrimType(Vector(strT), intT)
      case StrSubstrP() =>
        mkPrimType(Vector(strT, intT), strT)
      case StrGtP() | StrGteP() | StrLtP() | StrLteP() =>
        mkPrimType(Vector(strT, strT), boolT)
      case VecAppendP() =>
        mkVectorPrimTypeWithArgNames(
          Vector(("e", IdnT(IdnUse("t")))),
          VecT(IdnT(IdnUse("t")))
        )
      case VecConcatP() =>
        mkVectorPrimTypeWithArgNames(
          Vector(("vr", VecT(IdnT(IdnUse("t"))))),
          VecT(IdnT(IdnUse("t")))
        )
      case VecGetP() =>
        mkVectorPrimTypeWithArgNames(Vector(("i", intT)), IdnT(IdnUse("t")))
      case VecLengthP() =>
        mkVectorPrimTypeWithArgNames(Vector(), intT)
      case VecPrependP() =>
        mkVectorPrimTypeWithArgNames(
          Vector(("e", IdnT(IdnUse("t")))),
          VecT(IdnT(IdnUse("t")))
        )
      case VecPutP() =>
        mkVectorPrimTypeWithArgNames(
          Vector(("i", intT), ("e", IdnT(IdnUse("t")))),
          VecT(IdnT(IdnUse("t")))
        )
    }

  // Prelude

  def preludeStaticEnv(config: Config): Environment =
    if (config.noPrelude() || config.compilePrelude())
      rootenv()
    else {
      val path = s"${config.preludePath()}.static"
      loadPrelude(path).left.map { case (source, positions, message) =>
        val messaging = new Messaging(positions)
        config.output().emitln(s"cooma: can't read static prelude '$path'")
        messaging.report(source, Vector(message), config.output())
        sys.exit(1)
      }.merge
    }

  def loadPrelude(
      path: String
  ): Either[(Source, Positions, Message), Environment] = {
    val source =
      if ((new File(path)).isFile)
        FileSource(path)
      else {
        val stream = getClass.getClassLoader.getResourceAsStream(path)
        val br = new BufferedReader(new InputStreamReader(stream))
        val text = br.lines().collect(Collectors.joining("\n"))
        stream.close()
        StringSource(text)
      }
    val positions = new Positions
    val parser = new CoomaParser(source, positions)
    val result = parser.pStaticPrelude(0)
    if (result.hasValue) {
      val prelude = parser.value(result).asInstanceOf[StaticPrelude]
      val entries = prelude.optStaticPreludeEntrys
      Right(entries.foldLeft(rootenv()) {
        case (env, StaticTypedEntry(id, tipe)) =>
          define(env, id, PredefTypedEntity(id, tipe))
        case (env, StaticValEntry(id, tipe, exp)) =>
          define(env, id, PredefValEntity(id, tipe, exp))
        case (env, StaticTypeEntry(id, tipe)) =>
          define(env, id, PredefTypeEntity(id, tipe))
      })
    } else Left((source, positions, parser.errorToMessage(result.parseError)))
  }

}
