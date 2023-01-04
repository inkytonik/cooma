package org.bitbucket.inkytonik.cooma

import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
import org.bitbucket.inkytonik.kiama.util.Messaging._

import scala.annotation.tailrec

object TopLevelRewriter {

  def apply(program: Program): Either[Messages, Program] =
    program.expression match {
      case Blk(blockExp) =>
        @tailrec
        def aux(
            blockExp: BlockExp,
            lets: Vector[Let],
            defs: Vector[Def]
        ): Either[Messages, Program] =
          blockExp match {
            case BlkDef(Defs(newDefs), next) =>
              aux(next, lets, defs ++ newDefs)
            case BlkLet(let, next) =>
              aux(next, lets :+ let, defs)
            case Return(Fun(Arguments(args), body)) =>
              // main function
              if (defs.isEmpty) rewriteMain(lets, args, body).map(Program)
              else Left(defs.flatMap(error(_, "def not allowed here")))
            case Return(_) =>
              // nullary program
              Right(program)
          }
        aux(blockExp, Vector(), Vector())
      case _ =>
        Right(program)
    }

  def rewriteMain(
      lets: Seq[Let],
      args: Seq[Argument],
      body: Expression
  ): Either[Messages, Expression] =
    for {
      env <- rewriteLets(lets)
      argsr <- rewriteArgs(args, env)
    } yield {
      def aux(lets: Seq[Let]): BlockExp =
        lets match {
          case hd +: tl => BlkLet(hd, aux(tl))
          case _        => Return(body)
        }
      Fun(Arguments(argsr.toVector), Blk(aux(lets)))
    }

  def rewriteLets(lets: Seq[Let]): Either[Messages, Map[String, Expression]] = {
    val zero: Either[Messages, Map[String, Expression]] = Right(Map.empty)
    lets.foldLeft(zero) { case (out, Let(_, IdnDef(name), _, exp)) =>
      out.flatMap(env => rewrite(exp, env).map(exp => env + (name -> exp)))
    }
  }

  def rewriteArgs(
      args: Seq[Argument],
      env: Map[String, Expression]
  ): Either[Messages, Seq[Argument]] = {
    val rewritten = args.map { case Argument(idn, exp, doc) =>
      rewrite(exp, env).map(Argument(idn, _, doc))
    }
    validate(rewritten).left.map(_.toVector)
  }

  def rewrite(
      exp: Expression,
      env: Map[String, Expression]
  ): Either[Messages, Expression] = {
    // returns rewritten body (right) or sequence of illegal expressions (left)
    def aux(
        exp: Expression,
        env: Map[String, Expression]
    ): Either[Seq[Expression], Expression] =
      exp match {
        case Cat(e1, e2) =>
          val e1r = aux(e1, env)
          val e2r = aux(e2, env)
          (e1r, e2r) match {
            case (Right(e1r), Right(e2r)) => Right(Cat(e1r, e2r))
            case _                        => Left(collectErrors(Seq(e1r, e2r)))
          }
        case App(f, args) =>
          val fr = aux(f, env)
          val argsr = validate(args.map(aux(_, env)))
          (fr, argsr) match {
            case (Right(fr), Right(argsr)) => Right(App(fr, argsr.toVector))
            case _ => Left(collectErrors(Seq(fr, argsr)))
          }
        case RecT(flds) =>
          val fldsr = flds.map { case FieldType(name, exp) =>
            aux(exp, env).map(FieldType(name, _))
          }
          validate(fldsr).map(_.toVector).map(RecT)
        case VarT(flds) =>
          val fldsr = flds.map { case FieldType(name, exp) =>
            aux(exp, env).map(FieldType(name, _))
          }
          validate(fldsr).map(_.toVector).map(VarT)
        case VecNilT() =>
          Right(VecNilT())
        case VecT(e) =>
          aux(e, env).map(VecT)
        case Idn(IdnUse(idn)) =>
          env.get(idn) match {
            case Some(exp) => aux(exp, env)
            case None      => Right(exp)
          }
        case exp =>
          Left(Seq(exp))
      }
    aux(exp, env).left.map(_.flatMap(error(_, "not allowed here")).toVector)
  }

  /** Flattens a sequence of results whose failures may contain multiple errors.
    */
  def validate[A, B](
      results: Seq[Either[Seq[A], B]]
  ): Either[Seq[A], Seq[B]] = {
    val zero: Either[Seq[A], Seq[B]] = Right(Seq())
    results.foldLeft(zero) {
      case (Right(xs), Right(x))           => Right(xs :+ x)
      case (Right(_), Left(errors))        => Left(errors)
      case (Left(errors), Right(_))        => Left(errors)
      case (Left(errors), Left(newErrors)) => Left(errors ++ newErrors)
    }
  }

  /** Similar to `validate` but assumes the result is a failure. */
  def collectErrors[A](results: Seq[Either[Seq[A], Any]]): Seq[A] =
    results.flatMap {
      case Right(_) => Seq()
      case Left(xs) => xs
    }

}
