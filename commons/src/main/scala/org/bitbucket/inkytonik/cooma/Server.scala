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

trait Server {

  self: Driver =>

  import org.bitbucket.inkytonik.kiama.==>
  import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.{
    Document,
    emptyDocument
  }
  import org.bitbucket.inkytonik.kiama.util.{Position, Source}
  import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
  import org.bitbucket.inkytonik.cooma.PrettyPrinter._
  import org.bitbucket.inkytonik.cooma.SymbolTable._

  def getRelevantInfo[I](
      position: Position,
      f: (String, SemanticAnalyser, Vector[ASTNode]) ==> Option[I]
  ): Option[I] =
    for (
      analyser <- analysers.get(position.source);
      nodes = analyser.tree.nodes;
      relevantNodes = findNodesContaining(nodes, position);
      info <- f((position.source.name, analyser, relevantNodes))
    ) yield info

  // Make position checking slightly more permissive by allowing the finish
  // offset to be included. This means that in Code we can find definitions
  // etc from the position immediately after the idn, which seems to match
  // the behaviour of Code better than what Kiama does by default.

  def between[T](
      position: Position,
      t: T,
      start: Position,
      finish: Position
  ): Boolean =
    (start <= position) && (position <= finish)

  def findNodesContaining[T](nodes: Vector[T], position: Position): Vector[T] =
    nodes.collect(t =>
      (positions.getStart(t), positions.getFinish(t)) match {
        case (Some(start), Some(finish))
            if between(position, t, start, finish) =>
          t
      }
    )

  // Definition

  override def getDefinition(position: Position): Option[ASTNode] =
    getRelevantInfo(
      position,
      { case (uri, analyser, nodes) =>
        nodes
          .collectFirst {
            case n: IdnDef => analyser.defentity(n)
            case n: IdnUse => analyser.entity(n)
          }
          .toList
          .collectFirst { case e: CoomaOkEntity =>
            e.decl
          }
      }
    )

  // References

  override def getReferences(
      position: Position,
      includeDecl: Boolean
  ): Option[Vector[ASTNode]] =
    getRelevantInfo(
      position,
      { case (uri, analyser, nodes) =>
        nodes
          .collectFirst {
            case n: IdnDef => analyser.defentity(n)
            case n: IdnUse => analyser.entity(n)
          }
          .toList
          .collectFirst { case e: CoomaOkEntity =>
            val uses = analyser.tree.nodes.collect {
              case u: IdnUse if analyser.entity(u) == e =>
                u
            }
            if (includeDecl)
              idndefOfEntityDecl(e) match {
                case Some(decl) =>
                  decl +: uses
                case None =>
                  uses
              }
            else
              uses
          }
      }
    )

  def idndefOfEntityDecl(entity: CoomaOkEntity): Option[IdnDef] =
    entity match {
      case ArgumentEntity(decl)  => Some(decl.idnDef)
      case CaseValueEntity(decl) => Some(decl.idnDef)
      case FunctionEntity(decl)  => Some(decl.idnDef)
      case LetEntity(decl)       => Some(decl.idnDef)
      case _ =>
        None
    }

  // Hover

  override def getHover(position: Position): Option[String] =
    getRelevantInfo(
      position,
      { case (uri, analyser, nodes) =>
        nodes.collectFirst {
          case n: IdnDef =>
            idnDefHover(n, analyser)
          case n: IdnUse =>
            idnUseHover(n, analyser)
          case n: Field =>
            fieldHover(n, analyser)
          case n: FieldType =>
            fieldTypeHover(n, analyser)
          case n: Prm =>
            prmHover(n, analyser)
          case n: Sel =>
            selHover(n, analyser)
        }
      }
    )

  def fieldHover(n: Field, analyser: SemanticAnalyser): String = {
    analyser.tree.parent(n) match {
      case Vector(r: Rec) =>
        hoverMarkdown(
          s"record field ${n.identifier}",
          analyser.tipe(n.expression)
        )
      case Vector(v: Var) =>
        hoverMarkdown(
          s"variant field ${n.identifier}",
          analyser.tipe(n.expression)
        )
      case v =>
        s"fieldHover: Field $n doesn't have a sensible parent $v"
    }
  }

  def fieldTypeHover(n: FieldType, analyser: SemanticAnalyser): String = {
    analyser.tree.parent(n) match {
      case Vector(r: RecT) =>
        hoverMarkdown(
          s"record field type ${n.identifier}",
          analyser.unalias(n, n.expression)
        )
      case Vector(v: VarT) =>
        hoverMarkdown(
          s"variant field type ${n.identifier}",
          analyser.unalias(n, n.expression)
        )
      case v =>
        s"fieldTypeHover: FieldType $n doesn't have a sensible parent $v"
    }
  }

  def idnDefHover(n: IdnDef, analyser: SemanticAnalyser): String =
    entityHover(analyser.defentity(n), n.identifier, analyser)

  def idnUseHover(n: IdnUse, analyser: SemanticAnalyser): String =
    if (isPrimitiveTypeName(n.identifier))
      hoverMarkdown(s"reserved primitive type ${n.identifier}", Some(typT))
    else
      entityHover(analyser.entity(n), n.identifier, analyser)

  def entityHover(
      e: CoomaEntity,
      i: String,
      analyser: SemanticAnalyser
  ): String =
    e match {
      case MultipleEntity() =>
        s"multiply-defined $i"
      case UnknownEntity() =>
        s"unknown $i"
      case PredefLetEntity(name, tipe, value) =>
        hoverMarkdown(s"prelude $name", Some(tipe), Some(toDoc(value)))
      case PredefTypedEntity(name, tipe) =>
        hoverMarkdown(s"prelude $name", Some(tipe))
      case e: CoomaOkEntity =>
        hoverMarkdown(
          s"${e.desc} $i",
          analyser.entityType(e),
          Some(toDoc(e.decl))
        )
    }

  def prmHover(n: Prm, analyser: SemanticAnalyser): String = {
    val prim = n.userPrimitive
    val tipe = userPrimitiveType(prim)
    hoverMarkdown(s"primitive ${show(prim)}", Some(tipe))
  }

  def selHover(n: Sel, analyser: SemanticAnalyser): String = {
    val doc =
      toDoc(n.expression) <+> ":" <+> optToDoc(analyser.tipe(n.expression))
    hoverMarkdown(s"selection ${show(n)}", analyser.tipe(n), Some(doc))
  }

  def optToDoc(optNode: Option[ASTNode]): Doc =
    optNode match {
      case Some(n) =>
        toDoc(n)
      case None =>
        emptyDoc
    }

  def hoverMarkdown(
      title: String,
      tipe: Option[Expression] = None,
      optDoc: Option[Doc] = None
  ): String = {

    def md(rest: String): String = {
      val ts = tipe.map(show(_)).getOrElse("")
      s"### $title\n\n```cooma\n$ts\n$rest\n\n```"
    }

    optDoc match {
      case Some(doc) =>
        md(s"\n${layout(doc).trim()}\n")
      case None =>
        md("")
    }
  }

  def publishDesugaredTreeProduct(
      source: Source,
      document: => Document = emptyDocument
  ): Unit = {
    if (settingBool("showDesugaredSourceTree"))
      publishProduct(source, "desugaredtree", "scala", document)
  }

}
