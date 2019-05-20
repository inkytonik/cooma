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

class Interpreter(config : Config) {

    import org.bitbucket.inkytonik.cooma.Capabilities.capability
    import org.bitbucket.inkytonik.cooma.Primitives.primitive

    def interpret(term : Term, env : Env, args : List[String]) : ValueR = {

        def interpretAux(rho : Env, term : Term) : ValueR =
            term match {
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

                case Halt(x) =>
                    lookupR(rho, x)

                case LetC(k, x, t1, t2) =>
                    val rho2 = ConsCE(rho, k, ClsC(rho, x, t1))
                    interpretAux(rho2, t2)

                case LetF(ds, t) =>
                    lazy val rho2 : Env = ConsFE(rho, () => rho2, ds)
                    interpretAux(rho2, t)

                case LetV(x, v, t) =>
                    val rho2 = ConsVE(rho, x, interpretValue(v, rho))
                    interpretAux(rho2, t)
            }

        def interpretValue(value : Value, rho : Env) : ValueR =
            value match {
                case AndV(l, r) =>
                    lookupR(rho, l) match {
                        case RowR(lFields) =>
                            lookupR(rho, r) match {
                                case RowR(rFields) =>
                                    RowR(lFields ++ rFields)
                                case rv =>
                                    sys.error(s"interpretValue: right argument $r of & is non-row $rv")
                            }
                        case lv =>
                            sys.error(s"interpretValue: left argument $r of & is non-row $lv")
                    }

                case ArgV(i) =>
                    if ((i < 0) || (i >= args.length))
                        ErrR(s"command-line argument $i does not exist (arg count = ${args.length})")
                    else
                        StrR(args(i))

                case CapV(p, x) =>
                    capability(this, rho, p, x)

                case FunV(k, x, t) =>
                    ClsR(rho, k, x, t)

                case IntV(i) =>
                    IntR(i)

                case PrmV(name, args) =>
                    primitive(this, rho, name, args)

                case RowV(fields) =>
                    RowR(fields.map {
                        case FieldValue(f, v) =>
                            FldR(f, lookupR(rho, v))
                    })

                case SelV(x, f1) =>
                    lookupR(rho, x) match {
                        case RowR(fields) =>
                            fields.collectFirst {
                                case FldR(f2, v) if f1 == f2 =>
                                    v
                            } match {
                                case Some(v) =>
                                    v
                                case None =>
                                    sys.error(s"interpret SelV: can't find field $f1 in $fields")
                            }

                        case err : ErrR =>
                            err

                        case v =>
                            sys.error(s"interpret SelV: $x is $v, looking for field $f1")
                    }

                case StrV(s) =>
                    StrR(s)
            }

        val initEnv =
            ConsCE(
                env,
                "halt",
                ClsC(NilE(), "x", Halt("x"))
            )

        interpretAux(initEnv, term)
    }

    def lookupR(rho : Env, x : String) : ValueR =
        rho match {
            case ConsCE(rho2, _, _) =>
                lookupR(rho2, x)

            case ConsFE(rho2, rho2f, ds) =>
                ds.collectFirst {
                    case DefTerm(f, k, y, e) if x == f =>
                        ClsR(rho2f(), k, y, e)
                } match {
                    case Some(v) =>
                        v
                    case None =>
                        lookupR(rho2, x)
                }

            case ConsVE(rho2, y, v) if x == y =>
                v

            case ConsVE(rho2, _, _) =>
                lookupR(rho2, x)

            case NilE() =>
                sys.error(s"lookupR: can't find value $x")
        }

    def lookupC(rho : Env, x : String) : ClsC =
        rho match {
            case ConsCE(rho2, y, v) if x == y =>
                v

            case ConsCE(rho2, _, _) =>
                lookupC(rho2, x)

            case ConsFE(rho2, _, _) =>
                lookupC(rho2, x)

            case ConsVE(rho2, _, _) =>
                lookupC(rho2, x)

            case NilE() =>
                sys.error(s"lookupC: can't find $x")
        }

}
