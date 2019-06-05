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

    def interpret(term : Term, env : Env, args : Seq[String]) : ValueR = {

        def interpretAux(rho : Env, term : Term) : ValueR =
            term match {
                case AppC("$halt", x) =>
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
                    val rho2 = ConsCE(rho, k, ClsC(rho, x, t1))
                    interpretAux(rho2, t2)

                case LetF(ds, t) =>
                    lazy val rho2 : Env = ConsFE(rho, () => rho2, ds)
                    interpretAux(rho2, t)

                case LetV(x, v, t) =>
                    val rho2 : Env = ConsVE(rho, x, interpretValue(v, rho))
                    interpretAux(rho2, t)
            }

        def interpretValue(value : Value, rho : Env) : ValueR =
            value match {
                case FunV(k, x, t) =>
                    ClsR(rho, k, x, t)

                case IntV(i) =>
                    IntR(i)

                case PrmV(p, xs) =>
                    p.eval(this, rho, xs, args)

                case RowV(fields) =>
                    RowR(fields.map {
                        case FieldValue(f, v) =>
                            FldR(f, lookupR(rho, v))
                    })

                case StrV(s) =>
                    StrR(s)
            }

        interpretAux(env, term)
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
