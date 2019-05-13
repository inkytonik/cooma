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

object Interpreter {

    import java.nio.file.{Files, Paths}
    import org.bitbucket.inkytonik.cooma.Util.fresh
    import org.bitbucket.inkytonik.kiama.util.FileSource
    import syntax.CoomaParserPrettyPrinter.show
    import syntax.CoomaParserSyntax._

    def interpret(term : Term, args : List[String]) : ValueR = {

        def interpretAux(rho : Env, term : Term) : ValueR =
            term match {
                case AppC("halt", x) =>
                    lookupR(rho, x)

                case AppC(k, x) =>
                    lookupC(rho, k) match {
                        case ClsC(rho2, y, t) =>
                            interpretAux(ConsVE(rho, y, lookupR(rho, x)), t)

                        case v =>
                            sys.error(s"interpret AppC: $k is $v")
                    }

                case AppF(f, k, x) =>
                    lookupR(rho, f) match {
                        case ClsR(rho2, j, y, t) =>
                            interpretAux(
                                ConsCE(
                                    ConsVE(rho, y, lookupR(rho, x)),
                                    j,
                                    lookupC(rho, k)
                                ),
                                t
                            )

                        case err : ErrR =>
                            err

                        case v =>
                            sys.error(s"interpret AppF: $f is $v")
                    }

                case LetC(k, x, t1, t2) =>
                    interpretAux(ConsCE(rho, k, ClsC(rho, x, t1)), t2)

                case LetV(x, v, t) =>
                    interpretAux(ConsVE(rho, x, interpretValue(v, rho)), t)
            }

        def interpretValue(value : Value, rho : Env) : ValueR =
            value match {
                case ArgV(i) =>
                    if ((i < 0) || (i >= args.length))
                        sys.error(s"interpretValue: argument $i does not exist (max: ${args.length - 1})")
                    else
                        StrR(args(i))

                case CapV(p, x) =>
                    interpretCap(p, x, rho)

                case FldV(x, f1) =>
                    lookupR(rho, x) match {
                        case RowR(FldR(f2, v)) if f1 == f2 =>
                            v

                        case err : ErrR =>
                            err

                        case v =>
                            sys.error(s"interpret FldC: $x is $v, looking for field $f1")
                    }

                case FunV(k, x, t) =>
                    ClsR(rho, k, x, t)

                case IntV(i) =>
                    IntR(i)

                case PrmV(name, args) =>
                    (name, args) match {
                        case ("console", Vector(f, x)) =>
                            val s =
                                lookupR(rho, x) match {
                                    case StrR(s) =>
                                        s
                                    case v =>
                                        show(v)
                                }
                            Files.write(Paths.get(f), s.getBytes())
                            UniR()

                        case ("reader", Vector(f)) =>
                            StrR(FileSource(f).content)

                        case (name, arg) =>
                            sys.error(s"interpretValue: unknown primitive call $name $arg")
                    }

                case RowV(FieldValue(x, y)) =>
                    RowR(FldR(x, lookupR(rho, y)))

                case StrV(s) =>
                    StrR(s)

                case UniV() =>
                    UniR()
            }

        def interpretCap(name : String, x : String, rho : Env) : ValueR =
            name match {
                case "Console" =>
                    console(x, rho)
                case "Reader" =>
                    reader(x, rho)
                case _ =>
                    sys.error(s"interpretCap: unknown primitive $name")
            }

        interpretAux(NilE(), term)
    }

    def console(x : String, rho : Env) : ValueR =
        lookupR(rho, x) match {
            case StrR(s) =>
                if (Files.isWritable(Paths.get(s))) {
                    val k = fresh("k")
                    val y = fresh("y")
                    val p = fresh("p")
                    RowR(
                        FldR(
                            "write",
                            ClsR(NilE(), k, y,
                                LetV(p, PrmV("console", Vector(s, y)),
                                    AppC(k, p)))
                        )
                    )
                } else
                    ErrR(s"Console capability unavailable: can't write $s")

            case v =>
                sys.error(s"interpretPrim console: got non-String $v")
        }

    def reader(x : String, rho : Env) : ValueR =
        lookupR(rho, x) match {
            case StrR(s) =>
                if (Files.isReadable(Paths.get(s))) {
                    val k = fresh("k")
                    val y = fresh("y")
                    val p = fresh("p")
                    RowR(
                        FldR(
                            "read",
                            ClsR(NilE(), k, y,
                                LetV(p, PrmV("reader", Vector(s)),
                                    AppC(k, p)))
                        )
                    )
                } else
                    ErrR(s"Reader capability unavailable: can't read $s")

            case v =>
                sys.error(s"interpretPrim reader: got non-String $v")
        }

    def lookupR(rho : Env, x : String) : ValueR =
        rho match {
            case ConsCE(rho2, _, _) =>
                lookupR(rho2, x)

            case ConsVE(rho2, y, v) if x == y =>
                v

            case ConsVE(rho2, _, _) =>
                lookupR(rho2, x)

            case NilE() =>
                sys.error(s"lookupR: can't find $x")
        }

    def lookupC(rho : Env, x : String) : ValueC =
        rho match {
            case ConsCE(rho2, y, v) if x == y =>
                v

            case ConsCE(rho2, _, _) =>
                lookupC(rho2, x)

            case ConsVE(rho2, _, _) =>
                lookupC(rho2, x)

            case NilE() =>
                sys.error(s"lookupC: can't find $x")
        }

}
