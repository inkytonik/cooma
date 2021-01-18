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
    import org.bitbucket.inkytonik.kiama.output.PrettyPrinterTypes.Document
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
                        analyser.entity(n) match {
                            case MultipleEntity() =>
                                s"multiply-defined ${n.identifier}"
                            case UnknownEntity() =>
                                s"unknown ${n.identifier}"
                            case e : CoomaOkEntity =>
                                val title = s"${e.desc} ${n.identifier}"
                                val tipe =
                                    analyser.entityType(e) match {
                                        case Some(tipe) =>
                                            show(tipe)
                                        case _ =>
                                            ""
                                    }
                                val decl = hoverDocument(e.decl).layout
                                s"### $title\n\n```cooma\n$tipe\n\n$decl\n```"
                        }
                }
        })

    def hoverDocument(t : ASTNode) : Document = {

        def toHoverDoc(t : ASTNode) : Doc =
            t match {
                case a : Argument =>
                    toDoc(a)
                case d : Def =>
                    toDoc(d)
                case c : Case =>
                    toDoc(c)
                case v : Let =>
                    toDoc(v)
                case _ =>
                    emptyDoc
            }

        pretty(toHoverDoc(t))

    }

}
