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

class PreludeDriver extends Driver {

  import org.bitbucket.inkytonik.cooma.backend.ReferenceBackend
  import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
  import org.bitbucket.inkytonik.cooma.SymbolTable.{
    FunctionEntity,
    LetEntity,
    typT
  }
  import org.bitbucket.inkytonik.kiama.util.{FileEmitter, Source}
  import scala.collection.immutable.VectorBuilder

  /** Read the prelude and write the static and dynamic information files. The
    * static file contains names, types and values for prelude entities. The
    * dynamic file contains IR code for the whole prelude.
    */
  override def process(
      source: Source,
      program: Program,
      config: Config
  ): Unit = {
    val analyser = analysers(source)
    writeStaticPrelude(source, program, analyser, config)
    val messages = analyser.errors
    if (messages.length == 0)
      writeDynamicPrelude(source, program, config)
    else
      report(source, messages, config)
  }

  def writeStaticPrelude(
      source: Source,
      program: Program,
      analyser: SemanticAnalyser,
      config: Config
  ): Unit = {

    def entityIsType(entity: CoomaEntity): Boolean =
      analyser.entityType(entity) match {
        case Some(t) if t == typT =>
          true
        case Some(FunT(_, t)) if t == typT =>
          true
        case _ =>
          false
      }

    val env = analyser.deepEnv(program.expression)
    val builder = new VectorBuilder[StaticPreludeEntry]
    for (scope <- env.reverse) {
      for ((id, entity) <- scope) {
        analyser.entityType(entity) match {
          case Some(tipe) =>
            entity match {
              case LetEntity(decl) =>
                if (entityIsType(entity))
                  builder.addOne(StaticLetEntry(id, tipe, decl.expression))
                else
                  builder.addOne(StaticTypedEntry(id, tipe))
              case FunctionEntity(decl) =>
                if (entityIsType(entity))
                  builder.addOne(
                    StaticLetEntry(
                      id,
                      tipe,
                      Fun(decl.body.arguments, decl.body.expression2)
                    )
                  )
                else
                  builder.addOne(StaticTypedEntry(id, tipe))
              case _ =>
                sys.error(s"writeStaticPrelude: unexpected entity $entity")
            }
          case None =>
            sys.error(
              s"writeStaticPrelude: can't find type for $id entity $entity"
            )
        }
      }
    }

    val prelude = StaticPrelude(builder.result())
    val preludeText = PrettyPrinter.format(prelude).layout

    val filename = s"${source.name}.static"
    val emitter = new FileEmitter(filename)
    emitter.emit(preludeText)
    emitter.close()
    config.output().emitln(s"Wrote $filename")
  }

  def writeDynamicPrelude(
      source: Source,
      program: Program,
      config: Config
  ): Unit = {
    val system = new ReferenceBackend(this, source, config) with Compiler
    import system.{source => _, _}

    val prelude = compileCommand(program, positions, getAnalyser(source))
    val preludeText = PrettyPrinter.format(prelude, 5).layout
    val filename = s"${source.name}.dynamic"
    val emitter = new FileEmitter(filename)
    emitter.emitln(preludeText)
    emitter.close()
    config.output().emitln(s"Wrote $filename")
  }

}
