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

trait Backend extends Primitives {

    import java.io.Writer
    import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._

    type Term
    def appC(c : Cont, x : String) : Term
    def appF(f : String, k : String, x : String) : Term
    def casV(x : String, cs : Vector[CaseTerm]) : Term
    def letC(k : String, x : String, t : Term, body : Term) : Term
    def letF(ds : Vector[DefTerm], body : Term) : Term
    def letV(x : String, v : Value, body : Term) : Term

    type Cont
    def haltC() : Cont
    def idnC(k : String) : Cont

    type CaseTerm
    def caseTerm(c : String, k : String) : CaseTerm

    type DefTerm
    def defTerm(f : String, k : String, x : String, body : Term) : DefTerm

    type Value
    def funV(k : String, x : String, body : Term) : Value
    def intV(i : BigInt) : Value
    def prmV(p : Primitive, xs : Vector[String]) : Value
    def recV(fs : Vector[FldV]) : Value
    def strV(s : String) : Value
    def varV(f : FldV) : Value
    def vecV(e : Vector[String]) : Value

    type FldV
    def fldV(f : String, x : String) : FldV

    type ValueR
    def strR(str : String) : ValueR
    def varR(c : String, v : ValueR) : ValueR
    def intR(num : BigInt) : ValueR
    def clsR(f : String, x : String, env : Env, e : Term) : ValueR
    def recR(fields : Vector[FldR]) : ValueR
    def vecR(elems : Vector[ValueR]) : ValueR

    val uniR : ValueR = recR(Vector())
    val falseR : ValueR = varR("False", uniR)
    val trueR : ValueR = varR("True", uniR)

    def isStrR(value : ValueR) : Option[String]
    def isIntR(value : ValueR) : Option[BigInt]
    def isRecR(value : ValueR) : Option[Vector[FldR]]
    def isVarR(value : ValueR) : Option[(String, ValueR)]
    def isVecR(value : ValueR) : Option[Vector[ValueR]]

    type FldR
    def fldR(x : String, v : ValueR) : FldR
    def isFldR(value : FldR) : Option[(String, ValueR)]
    def getFieldName(value : FldR) : String
    def getFieldValue(value : FldR) : ValueR

    def showTerm(t : Term) : String
    def showRuntimeValue(v : OutputValueR) : String

    def backendName : String

    type Env
    def emptyEnv : Env

    def lookupR(rho : Env, x : String) : ValueR
    // def insertR TODO

    def stdout : Writer
    def getConfig : Config

    case class Result(rho : Env, value : ValueR)
    def interpret(term : Term, rho : Env, args : Seq[String], config : Config) : Either[String, Result]

    /**
     * When evaluating a program using Truffle, what we get as an output
     * are org.graalvm.polyglot.Value values instead of ValueRs,
     * therefore, we need to have this second output type.
     * For the reference interpreter, this type will point to the ValueR and
     * in the case of the truffle, to org.graalvm.polyglot.Value
     */
    type OutputValueR

}
