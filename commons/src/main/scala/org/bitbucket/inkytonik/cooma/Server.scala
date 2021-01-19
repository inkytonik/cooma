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

    self : Driver =>

    import org.bitbucket.inkytonik.kiama.==>
    import org.bitbucket.inkytonik.kiama.util.Position
    import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
    import org.bitbucket.inkytonik.cooma.CoomaParserPrettyPrinter._
    import org.bitbucket.inkytonik.cooma.SymbolTable._

    def getRelevantInfo[I](
        position : Position,
        f : (String, SemanticAnalyser, Vector[ASTNode]) ==> Option[I]
    ) : Option[I] =
        for (
            analyser <- analysers.get(position.source);
            nodes = analyser.tree.nodes;
            relevantNodes = positions.findNodesContaining(nodes, position);
            info <- f((position.source.name, analyser, relevantNodes))
        ) yield info

    override def getHover(position : Position) : Option[String] =
        getRelevantInfo(position, {
            case (uri, analyser, nodes) =>
                nodes.collectFirst {
                    case n : IdnUse =>
                        idnUseHover(n, analyser)
                    case n : Field =>
                        fieldHover(n, analyser)
                    case n : FieldType =>
                        fieldTypeHover(n, analyser)
                    case n : Prm =>
                        prmHover(n, analyser)
                    case n : Sel =>
                        selHover(n, analyser)
                }
        })

    def fieldHover(n : Field, analyser : SemanticAnalyser) : String = {
        analyser.tree.parent(n) match {
            case Vector(r : Rec) =>
                hoverMarkdown(s"record field ${n.identifier}", analyser.tipe(n.expression))
            case Vector(v : Var) =>
                hoverMarkdown(s"variant field ${n.identifier}", analyser.tipe(n.expression))
            case v =>
                s"fieldHover: Field $n doesn't have a sensible parent $v"
        }
    }

    def fieldTypeHover(n : FieldType, analyser : SemanticAnalyser) : String = {
        analyser.tree.parent(n) match {
            case Vector(r : RecT) =>
                hoverMarkdown(s"record field type ${n.identifier}", analyser.unalias(n, n.expression))
            case Vector(v : VarT) =>
                hoverMarkdown(s"variant field type ${n.identifier}", analyser.unalias(n, n.expression))
            case v =>
                s"fieldTypeHover: FieldType $n doesn't have a sensible parent $v"
        }
    }

    def idnUseHover(n : IdnUse, analyser : SemanticAnalyser) : String =
        analyser.entity(n) match {
            case MultipleEntity() =>
                s"multiply-defined ${n.identifier}"
            case UnknownEntity() =>
                s"unknown ${n.identifier}"
            case e : CoomaOkEntity =>
                hoverMarkdown(s"${e.desc} ${n.identifier}", analyser.entityType(e),
                    Some(toDoc(e.decl)))
        }

    def prmHover(n : Prm, analyser : SemanticAnalyser) : String = {
        val i = n.identifier
        hoverMarkdown(s"primitive $i", primitivesTypesTable.get(i))
    }

    def selHover(n : Sel, analyser : SemanticAnalyser) : String = {
        val doc = toDoc(n.expression) <+> ":" <+> optToDoc(analyser.tipe(n.expression))
        hoverMarkdown(s"selection ${layout(toDoc(n))}", analyser.tipe(n), Some(doc))
    }

    def optToDoc(optNode : Option[ASTNode]) : Doc =
        optNode match {
            case Some(n) =>
                toDoc(n)
            case None =>
                emptyDoc
        }

    def hoverMarkdown(title : String, tipe : Option[Expression] = None,
        optDoc : Option[Doc] = None) : String = {

        def md(rest : String) : String = {
            val ts = tipe.map(show(_)).getOrElse("")
            s"### $title\n\n```cooma\n$ts\n$rest```"
        }

        optDoc match {
            case Some(doc) =>
                md(s"\n${layout(doc).trim()}\n")
            case None =>
                md("")
        }
    }

}
