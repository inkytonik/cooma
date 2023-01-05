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

import org.bitbucket.inkytonik.cooma.primitive.database.Metadata

trait Compiler {

  self: Backend =>

  import org.bitbucket.inkytonik.cooma.CoomaParserSyntax.Program
  import org.bitbucket.inkytonik.cooma.Util.resetFresh
  import org.bitbucket.inkytonik.kiama.util.Positions

  def compileCommand(
      prog: Program,
      positions: Positions,
      analyser: SemanticAnalyser
  ): Term = {
    resetFresh()
    val compiler = new CompilerCore(positions, analyser)
    compiler.compileTop(prog.expression, 0)
  }

  def compileStandalone(
      prog: Program,
      positions: Positions,
      analyser: SemanticAnalyser
  ): Term = {
    resetFresh()
    val compiler = new CompilerCore(positions, analyser)
    compiler.compileHalt(prog.expression)
  }

  class CompilerCore(positions: Positions, analyser: SemanticAnalyser) {

    import org.bitbucket.inkytonik.cooma.CoomaParserSyntax.{
      CaseTerm => _,
      Cont => _,
      DefTerm => _,
      FldV => _,
      Term => _,
      Value => _,
      _
    }
    import org.bitbucket.inkytonik.cooma.PrettyPrinter.show
    import org.bitbucket.inkytonik.cooma.SymbolTable.{
      PrimitiveType,
      StrT,
      capabilityTypeNames,
      uniT
    }
    import org.bitbucket.inkytonik.cooma.Util.fresh

    /** Case class and map that stores primitives metadata.
      */
    case class PrimitiveMeta(prm: UserPrimitive)

    val primitivesTable = Map(
      "Equal" -> PrimitiveMeta(EqualP()),
      "IntAbs" -> PrimitiveMeta(IntAbsP()),
      "IntAdd" -> PrimitiveMeta(IntAddP()),
      "IntSub" -> PrimitiveMeta(IntSubP()),
      "IntMul" -> PrimitiveMeta(IntMulP()),
      "IntDiv" -> PrimitiveMeta(IntDivP()),
      "IntPow" -> PrimitiveMeta(IntPowP()),
      "IntGt" -> PrimitiveMeta(IntGtP()),
      "IntGte" -> PrimitiveMeta(IntGteP()),
      "IntLt" -> PrimitiveMeta(IntLtP()),
      "IntLte" -> PrimitiveMeta(IntLteP()),
      "StrConcat" -> PrimitiveMeta(StrConcatP()),
      "StrLength" -> PrimitiveMeta(StrLengthP()),
      "StrSubstr" -> PrimitiveMeta(StrSubstrP()),
      "StrGt" -> PrimitiveMeta(StrGtP()),
      "StrGte" -> PrimitiveMeta(StrGteP()),
      "StrLt" -> PrimitiveMeta(StrLtP()),
      "StrLte" -> PrimitiveMeta(StrLteP()),
      "VecAppend" -> PrimitiveMeta(VecAppendP()),
      "VecConcat" -> PrimitiveMeta(VecConcatP()),
      "VecGet" -> PrimitiveMeta(VecGetP()),
      "VecLength" -> PrimitiveMeta(VecLengthP()),
      "VecPrepend" -> PrimitiveMeta(VecPrependP()),
      "VecPut" -> PrimitiveMeta(VecPutP())
    )

    // Tree node construction wrappers that copy source locations

    def mkAppC(source: ASTNode, c: Cont, x: String): Term =
      positions.dupPos(source, appC(c, x))

    def mkAppF(source: ASTNode, f: String, k: String, x: String): Term =
      positions.dupPos(source, appF(f, k, x))

    def mkCasV(source: ASTNode, x: String, cs: Vector[CaseTerm]): Term =
      positions.dupPos(source, casV(x, cs))

    def mkLetC(
        source: ASTNode,
        k: String,
        x: String,
        t: Term,
        body: Term
    ): Term =
      positions.dupPos(source, letC(k, x, t, body))

    def mkLetF(source: ASTNode, ds: Vector[DefTerm], body: Term): Term =
      positions.dupPos(source, letF(ds, body))

    def mkLetV(source: ASTNode, x: String, v: Value, body: Term): Term =
      positions.dupPos(source, letV(x, v, body))

    def mkCaseTerm(source: ASTNode, c: String, k: String): CaseTerm =
      positions.dupPos(source, caseTerm(c, k))

    def mkDefTerm(
        source: ASTNode,
        f: String,
        k: String,
        x: String,
        body: Term
    ): DefTerm =
      positions.dupPos(source, defTerm(f, k, x, body))

    /** Compile an expression to produce its value via the halt continuation.
      */
    def compileHalt(exp: Expression): Term =
      compile(exp, z => mkAppC(exp, haltC(), z))

    def compileTop(exp: Expression, nArg: Int): Term = {

      def compileTopArg(
          arg: Argument,
          a: String,
          t: Expression,
          e: Expression
      ): Term = {

        def compileCapArg(n: List[String]): Term = {
          val x = fresh("x")
          def aux(n: List[String], prev: String): Term = {
            val c0 = fresh("c")
            n match {
              case hd :: Nil =>
                mkLetV(
                  arg,
                  c0,
                  prmV(CapabilityP(hd), Vector(x)),
                  mkLetV(
                    arg,
                    a,
                    prmV(RecConcatP(), Vector(prev, c0)),
                    compileTop(e, nArg + 1)
                  )
                )
              case hd :: (tl @ _ :: _) =>
                val c1 = fresh("c")
                mkLetV(
                  arg,
                  c0,
                  prmV(CapabilityP(hd), Vector(x)),
                  mkLetV(
                    arg,
                    c1,
                    prmV(RecConcatP(), Vector(prev, c0)),
                    aux(tl, c1)
                  )
                )
              case Nil =>
                sys.error("compileCapArg: unexpected Nil")
            }
          }
          n match {
            case hd :: Nil =>
              mkLetV(
                arg,
                x,
                prmV(ArgumentP(nArg), Vector()),
                mkLetV(
                  arg,
                  a,
                  prmV(CapabilityP(hd), Vector(x)),
                  compileTop(e, nArg + 1)
                )
              )
            case hd :: tl =>
              val c = fresh("c")
              mkLetV(
                arg,
                x,
                prmV(ArgumentP(nArg), Vector()),
                mkLetV(arg, c, prmV(CapabilityP(hd), Vector(x)), aux(tl, c))
              )
            case Nil =>
              sys.error("compileCapArg: unexpected Nil")
          }
        }

        t match {
          case StrT() =>
            mkLetV(
              arg,
              a,
              prmV(ArgumentP(nArg), Vector()),
              compileTop(e, nArg + 1)
            )
          case t =>
            def aux(t: Expression): List[String] =
              t match {
                case Cat(e1, e2) =>
                  aux(e1) ::: aux(e2)
                case Idn(IdnUse(name)) if capabilityTypeNames(name) =>
                  name :: Nil
                case App(Idn(IdnUse("Database")), Vector(RecT(ts))) =>
                  Metadata.fromCooma(ts) match {
                    case Some(metadata) =>
                      s"DatabaseClient:$nArg:${metadata.toSpec}" :: Nil
                    case None =>
                      sys.error(
                        s"compileTopArg: invalid database type ${show(t)}"
                      )
                  }
                case App(Idn(IdnUse("HttpServer")), Vector(RecT(ts))) =>
                  "HttpServer" :: Nil
                case t =>
                  sys.error(
                    s"compileTopArg: ${show(t)} arguments not supported"
                  )
              }
            compileCapArg(aux(t))
        }

      }

      val result =
        exp match {
          case Fun(Arguments(Vector()), e) =>
            compileHalt(e)
          case Fun(Arguments(Vector(arg @ Argument(IdnDef(a), t, _))), e) =>
            compileTopArg(arg, a, t, e)
          case Fun(Arguments((arg @ Argument(IdnDef(a), t, _)) +: as), e) =>
            compileTopArg(arg, a, t, Fun(Arguments(as), e))
          case _ =>
            compileHalt(exp)
        }

      if (nArg == 0) {
        val numArgs =
          exp match {
            case Fun(Arguments(args), _) => args.length
            case _                       => 0
          }
        mkLetV(exp, "_", prmV(ArgumentCheckP(numArgs), Vector()), result)
      } else result
    }

    /** Short-hand for compiling where the source expression and the expression
      * being compiled are the same.
      */
    def compile(exp: Expression, kappa: String => Term): Term =
      compile(exp, exp, kappa)

    /* Unit is encoded as an empty record. */
    val uniV = recV(Vector())

    /** Compile `exp`. The compiled code will be related to the `source` node.
      * In many cases these will be the same, but where we desguar `source` will
      * be the original node, whereas `exp` will be the desugared version.
      */
    def compile(
        source: Expression,
        exp: Expression,
        kappa: String => Term
    ): Term =
      exp match {
        case App(f, Vector()) =>
          compile(exp, App(f, Vector(Uni())), kappa)

        case App(f, Vector(a)) =>
          val k = fresh("k")
          val r = fresh("r")
          compile(
            f,
            y =>
              compile(
                a,
                z => mkLetC(source, k, r, kappa(r), mkAppF(source, y, k, z))
              )
          )

        case App(f, a +: as) =>
          compile(exp, App(App(f, Vector(a)), as), kappa)

        case Blk(be) =>
          compileBlockExp(be, kappa)

        case Cat(r1, r2) =>
          val r = fresh("r")
          compile(
            r1,
            y =>
              compile(
                r2,
                z =>
                  mkLetV(source, r, prmV(RecConcatP(), Vector(y, z)), kappa(r))
              )
          )

        case Fun(Arguments(Vector()), e) =>
          compileFun(exp, "_", uniT, e, kappa)

        case Fun(Arguments(Vector(Argument(IdnDef(x), t, _))), e) =>
          compileFun(exp, x, t, e, kappa)

        case Fun(Arguments(Argument(IdnDef(x), t, _) +: as), e) =>
          compileFun(exp, x, t, Fun(Arguments(as), e), kappa)

        case Type() =>
          compile(exp, Uni(), kappa)

        case Idn(IdnUse(i)) =>
          kappa(i)

        case Mat(e, cs) =>
          compileMatch(exp, e, cs, kappa)

        case Num(n) =>
          val i = fresh("i")
          mkLetV(source, i, intV(n), kappa(i))

        case Prm(p, args) =>
          val r = fresh("r")
          compilePrimArgs(
            args,
            cArgs => mkLetV(source, r, prmV(UserP(p), cArgs), kappa(r))
          )

        case Rec(fields) =>
          val r = fresh("r")
          compileRec(fields, fvs => mkLetV(source, r, recV(fvs), kappa(r)))

        case Sel(r, FieldUse(s)) =>
          val f = fresh("f")
          compile(
            r,
            z => mkLetV(source, f, prmV(RecSelectP(), Vector(z, s)), kappa(f))
          )

        case Str(l) =>
          val s = fresh("s")
          mkLetV(source, s, strV(l.tail.init), kappa(s))

        case Uni() =>
          val u = fresh("u")
          mkLetV(source, u, uniV, kappa(u))

        case v @ Var(field) =>
          val r = fresh("r")
          compile(
            source,
            field.expression,
            z => mkLetV(source, r, varV(fldV(field.identifier, z)), kappa(r))
          )

        case Vec(VecElems(e)) =>
          val v = fresh("v")
          compilePrimArgs(e, elems => mkLetV(source, v, vecV(elems), kappa(v)))

        case _ =>
          sys.error(s"compile: unexpected expression $exp")
      }

    object Type {
      def unapply(e: Expression): Boolean =
        e match {
          case _: FunT | _: RecT | _: VarT | _: VecT | _: VecNilT |
              PrimitiveType() =>
            true
          case _ =>
            false
        }
    }

    def compileFun(
        exp: Expression,
        x: String,
        t: Expression,
        e: Expression,
        kappa: String => Term
    ): Term = {
      val f = fresh("f")
      val j = fresh("k")
      mkLetV(exp, f, funV(j, x, tailCompile(e, j)), kappa(f))
    }

    def compileBlockExp(be: BlockExp, kappa: String => Term): Term =
      be match {
        case BlkDef(ds, be2) =>
          mkLetF(
            be,
            ds.defs.map(compileDef),
            compileBlockExp(be2, kappa)
          )

        case BlkLet(Let(_, IdnDef(x), _, e), be2) =>
          val j = fresh("k")
          mkLetC(be, j, x, compileBlockExp(be2, kappa), tailCompile(e, j))

        case Return(e) =>
          compile(e, kappa)
      }

    def compileDefBody(args: Vector[Argument], e: Expression, k: String): Term =
      if (args.isEmpty)
        tailCompile(e, k)
      else
        tailCompile(Fun(Arguments(args), e), k)

    def compileDef(fd: Def): DefTerm = {
      val k = fresh("k")
      fd match {
        case Def(IdnDef(f), Body(Arguments(Vector()), t, e)) =>
          compileDef(
            Def(
              IdnDef(f),
              Body(Arguments(Vector(Argument(IdnDef("_"), uniT, None))), t, e)
            )
          )

        case Def(
              IdnDef(f),
              Body(Arguments(Argument(IdnDef(x), _, None) +: otherArgs), _, e)
            ) =>
          mkDefTerm(fd, f, k, x, compileDefBody(otherArgs, e, k))

        case _ =>
          sys.error(s"compileDef: unexpected definition $fd")
      }
    }

    def compileMatch(
        exp: Expression,
        e: Expression,
        cs: Vector[Case],
        kappa: String => Term
    ): Term = {
      val cks = cs.map(c => (c, fresh("k")))
      val caseTerms =
        cks.map { case (c, k) =>
          mkCaseTerm(c, c.identifier, k)
        }
      compile(
        e,
        z =>
          cks.foldLeft(mkCasV(exp, z, caseTerms)) {
            case (t, (Case(_, IdnDef(xi), ei), ki)) =>
              mkLetC(ei, ki, xi, compile(ei, zi => kappa(zi)), t)
          }
      )
    }

    def compileRec(
        fields: Vector[Field],
        kappa: Vector[FldV] => Term
    ): Term =
      fields match {
        case Field(f, e) +: t =>
          compile(e, z => compileRec(t, fvs => kappa(fldV(f, z) +: fvs)))

        case Vector() =>
          kappa(Vector())

        case _ =>
          sys.error(s"compileRec: unexpected fields $fields")
      }

    def compilePrimArgs(
        args: Vector[Expression],
        kappa: Vector[String] => Term
    ): Term =
      args match {
        case e +: t =>
          compile(e, argE => compilePrimArgs(t, argT => kappa(argE +: argT)))

        case Vector() =>
          kappa(Vector())

        case _ =>
          sys.error(s"compilePrimArgs: unexpected fields $args")
      }

    def tailCompile(exp: Expression, k: String): Term =
      tailCompile(exp, exp, k)

    def tailCompile(source: Expression, exp: Expression, k: String): Term =
      exp match {
        case App(f, Vector()) =>
          tailCompile(exp, App(f, Vector(Uni())), k)

        case App(f, Vector(a)) =>
          compile(f, y => compile(a, z => mkAppF(source, y, k, z)))

        case App(f, a +: as) =>
          tailCompile(exp, App(App(f, Vector(a)), as), k)

        case Blk(be) =>
          tailCompileBlockExp(be, k)

        case Cat(r1, r2) =>
          val r = fresh("r")
          compile(
            r1,
            y =>
              compile(
                r2,
                z =>
                  mkLetV(
                    source,
                    r,
                    prmV(RecConcatP(), Vector(y, z)),
                    mkAppC(source, idnC(k), r)
                  )
              )
          )

        case Fun(Arguments(Vector()), e) =>
          tailCompileFun(exp, "_", uniT, e, k)

        case Fun(Arguments(Vector(Argument(IdnDef(x), t, _))), e) =>
          tailCompileFun(exp, x, t, e, k)

        case Fun(Arguments(Argument(IdnDef(x), t, _) +: as), e) =>
          tailCompileFun(exp, x, t, Fun(Arguments(as), e), k)

        case Fun(Arguments(a +: as), e) =>
          tailCompile(exp, Fun(Arguments(Vector(a)), Fun(Arguments(as), e)), k)

        case Type() =>
          tailCompile(exp, Uni(), k)

        case Idn(IdnUse(x)) =>
          mkAppC(source, idnC(k), x)

        case Mat(e, cs) =>
          tailCompileMatch(exp, e, cs, k)

        case Num(n) =>
          val i = fresh("i")
          mkLetV(source, i, intV(n), mkAppC(source, idnC(k), i))

        case Prm(p, args) =>
          val r = fresh("r")
          compilePrimArgs(
            args,
            cArgs =>
              mkLetV(
                source,
                r,
                prmV(UserP(p), cArgs),
                mkAppC(source, idnC(k), r)
              )
          )

        case Rec(fields) =>
          val r = fresh("r")
          compileRec(
            fields,
            fvs => mkLetV(source, r, recV(fvs), mkAppC(source, idnC(k), r))
          )

        case Sel(r, FieldUse(s)) =>
          val f = fresh("f")
          compile(
            r,
            z =>
              mkLetV(
                source,
                f,
                prmV(RecSelectP(), Vector(z, s)),
                mkAppC(source, idnC(k), f)
              )
          )

        case Str(l) =>
          val s = fresh("s")
          mkLetV(source, s, strV(l.tail.init), mkAppC(source, idnC(k), s))

        case Uni() =>
          val u = fresh("u")
          mkLetV(source, u, recV(Vector()), mkAppC(source, idnC(k), u))

        case Var(field) =>
          val r = fresh("r")
          compile(
            source,
            field.expression,
            z =>
              mkLetV(
                source,
                r,
                varV(fldV(field.identifier, z)),
                mkAppC(source, idnC(k), r)
              )
          )

        case Vec(e) =>
          val v = fresh("v")
          compilePrimArgs(
            e.optExpressions,
            elems => mkLetV(source, v, vecV(elems), mkAppC(source, idnC(k), v))
          )

        case _ =>
          sys.error(s"tailCompile: unexpected expression $exp")

      }

    def tailCompileFun(
        exp: Expression,
        x: String,
        t: Expression,
        e: Expression,
        k: String
    ): Term = {
      val f = fresh("f")
      val j = fresh("k")
      mkLetV(exp, f, funV(j, x, tailCompile(e, j)), mkAppC(exp, idnC(k), f))
    }

    def tailCompileBlockExp(be: BlockExp, k: String): Term =
      be match {
        case BlkDef(ds, be2) =>
          mkLetF(
            be,
            ds.defs.map(compileDef),
            tailCompileBlockExp(be2, k)
          )

        case BlkLet(Let(_, IdnDef(x), _, e), be2) =>
          val j = fresh("k")
          mkLetC(be, j, x, tailCompileBlockExp(be2, k), tailCompile(e, j))

        case Return(e) =>
          tailCompile(e, k)
      }

    def tailCompileMatch(
        exp: Expression,
        e: Expression,
        cs: Vector[Case],
        k: String
    ): Term = {
      val cks = cs.map(c => (c, fresh("k")))
      val caseTerms =
        cks.map { case (c, k) =>
          mkCaseTerm(c, c.identifier, k)
        }
      compile(
        e,
        z =>
          cks.foldLeft(mkCasV(exp, z, caseTerms)) {
            case (t, (Case(vi, IdnDef(xi), ei), ki)) =>
              mkLetC(ei, ki, xi, tailCompile(ei, k), t)
          }
      )

    }
  }

}
