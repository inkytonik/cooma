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

import org.bitbucket.inkytonik.cooma.CoomaParserSyntax.ASTNode
import org.bitbucket.inkytonik.cooma.SymbolTable._
import org.bitbucket.inkytonik.kiama.attribution.Attribution
import org.bitbucket.inkytonik.kiama.relation.Tree

class SemanticAnalyser(
    val tree: Tree[ASTNode, ASTNode],
    predefStaticEnv: Environment = rootenv()
) extends Attribution {

  import org.bitbucket.inkytonik.kiama.util.Messaging.{Messages, noMessages}
  import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._

  lazy val errors: Messages =
    noMessages

  def tipe(exp: Expression): Option[Type] =
    None

  def replType(input: REPLInput): Option[Type] =
    None

  def replTypeValue(input: REPLInput): Option[Expression] =
    None

  def unalias[T](n: ASTNode, m: T): Option[T] =
    Some(m)

  def entity(idn: IdnUse): CoomaEntity =
    UnknownEntity()

  def defentity(idn: IdnDef): CoomaEntity =
    UnknownEntity()

  def entityType(entity: CoomaEntity): Option[Type] =
    None

  def deepEnv(exp: Expression): Environment =
    rootenv()

  def subtype(t: Type, u: Type): Boolean =
    true

  def subtypes(ts: Vector[Type], us: Vector[Type]): Boolean =
    true

  sealed trait BoundResult extends Product
  case object IgnoreBound extends BoundResult
  case object NoBound extends BoundResult
  case class Bound(tipe: Type) extends BoundResult

  sealed trait BoundDirection extends Product {
    def bound =
      this match {
        case Lub => lub _
        case Glb => glb _
      }
  }

  case object Lub extends BoundDirection
  case object Glb extends BoundDirection

  def getLub(es: Vector[Type]): BoundResult =
    NoBound

  def lub(t: Type, u: Type): Option[Type] =
    Some(t)

  def glb(t: Type, u: Type): Option[Type] =
    Some(t)

}
