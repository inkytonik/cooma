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

trait Backend {

    import org.bitbucket.inkytonik.cooma.CoomaParserSyntax.ASTNode
    import org.bitbucket.inkytonik.kiama.relation.Bridge

    type Term
    def appC(source : Bridge[ASTNode], k : String, x : String) : Term
    def appF(source : Bridge[ASTNode], f : String, k : String, x : String) : Term
    def casV(source : Bridge[ASTNode], x : String, cs : Vector[CaseTerm]) : Term
    def letC(source : Bridge[ASTNode], k : String, x : String, t : Term, body : Term) : Term
    def letF(source : Bridge[ASTNode], ds : Vector[DefTerm], body : Term) : Term
    def letV(source : Bridge[ASTNode], x : String, v : Value, body : Term) : Term

    type CaseTerm
    def caseTerm(source : Bridge[ASTNode], c : String, k : String) : CaseTerm

    type DefTerm
    def defTerm(source : Bridge[ASTNode], f : String, k : String, x : String, body : Term) : DefTerm

    type Value
    def funV(k : String, x : String, body : Term) : Value
    def intV(i : BigInt) : Value
    def prmV(p : Primitive, xs : Vector[String]) : Value
    def recV(fs : Vector[FieldValue]) : Value
    def strV(s : String) : Value
    def varV(v : String, x : String) : Value

    type FieldValue
    def fieldValue(f : String, x : String) : FieldValue

    type Primitive
    def argumentP(i : Int) : Primitive
    def capabilityP(cap : String) : Primitive
    def readerReadP(filename : String) : Primitive
    def recConcatP() : Primitive
    def recSelectP() : Primitive
    def writerWriteP(filename : String) : Primitive

    def equalP : Primitive
    def intBinP(op : Primitives.IntPrimBinOp) : Primitive
    def intRelP(op : Primitives.IntPrimRelOp) : Primitive
    def stringP(op : Primitives.StrPrimOp) : Primitive

    def showTerm(t : Term) : String

    type ValueR
    def errR(msg : String) : ValueR
    def strR(str : String) : ValueR
    def varR(c : String, v : ValueR) : ValueR
    def intR(num : BigInt) : ValueR
    def clsR(source : Bridge[ASTNode], env : Env, f : String, x : String, e : Term) : ValueR
    def recR(fields : Vector[FldR]) : ValueR

    val unitR : ValueR = recR(Vector())
    val falseR : ValueR = varR("False", unitR)
    val trueR : ValueR = varR("True", unitR)

    def isErrR(value : ValueR) : Option[String]
    def isStrR(value : ValueR) : Option[String]
    def isIntR(value : ValueR) : Option[BigInt]
    def isRecR(value : ValueR) : Option[Vector[FldR]]
    def isVarR(value : ValueR) : Option[(String, ValueR)]

    type FldR
    def fldR(x : String, v : ValueR) : FldR
    def isFldR(value : FldR) : Option[(String, ValueR)]
    def getFieldName(value : FldR) : String
    def getFieldValue(value : FldR) : ValueR

    def showRuntimeValue(v : OutputValueR) : String

    def backendName : String

    type Env
    def emptyEnv : Env

    def lookupR(rho : Env, x : String) : ValueR

    def getConfig : Config

    /**
     * When evaluating a program using Truffle, what we get as an output
     * are org.graalvm.polyglot.Value values instead of ValueRs,
     * therefore, we need to have this second output type.
     * For the reference interpreter, this type will point to the ValueR and
     * in the case of the truffle, to org.graalvm.polyglot.Value
     */
    type OutputValueR

}
