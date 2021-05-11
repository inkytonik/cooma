/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.cooma.truffle

import org.bitbucket.inkytonik.cooma.{Backend, Config}

class TruffleBackend(
    config : Config
) extends Backend {

    import java.io.{PrintWriter, Writer}
    import java.math.BigInteger
    import org.bitbucket.inkytonik.cooma.CoomaParserSyntax.Primitive

    import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho
    import org.bitbucket.inkytonik.cooma.truffle.nodes.term._
    import org.bitbucket.inkytonik.cooma.truffle.nodes.value._
    import org.bitbucket.inkytonik.cooma.truffle.runtime._

    import scala.math.BigInt

    override def backendName : String = "Graal VM"

    // Terms

    override type Term = CoomaTermNode

    override type Value = CoomaValueNode

    def appC(k : Cont, x : String) : CoomaTermNode =
        CoomaAppCTermNodeGen.create(k, x)

    def appF(f : String, k : String, x : String) : CoomaTermNode =
        CoomaAppFTermNodeGen.create(f, k, x)

    type CaseTerm = CoomaCaseTerm

    def casV(x : String, cs : Vector[CaseTerm]) : CoomaTermNode =
        new CoomaCasVTermNode(x, cs.toArray)

    def letC(k : String, x : String, t : Term, body : Term) : CoomaTermNode =
        new CoomaLetCTermNode(k, x, t, body)

    type DefTerm = CoomaDefTerm

    def letF(ds : Vector[DefTerm], body : Term) : CoomaTermNode =
        new CoomaLetFTermNode(ds.toArray, body)

    def letV(x : String, v : Value, body : Term) : Term =
        new CoomaLetVTermNode(x, v, body)

    def caseTerm(c : String, k : String) : CaseTerm =
        new CoomaCaseTerm(c, k)

    def defTerm(f : String, k : String, x : String, body : Term) : DefTerm =
        new CoomaDefTerm(f, k, x, body)

    type Cont = CoomaCont

    def haltC() : Cont =
        new CoomaHaltC()

    def idnC(k : String) : Cont =
        new CoomaIdnC(k)

    // Values

    def funV(k : String, x : String, body : Term) : Value =
        new CoomaFunctionValueNode(k, x, body)

    def intV(i : BigInt) : Value =
        new CoomaIntValueNode(i.bigInteger)

    def prmV(p : Primitive, xs : Vector[String]) : Value =
        new CoomaPrimitiveValue(this, p, xs.toArray)

    def recV(fs : Vector[CoomaFldV]) : Value =
        new CoomaRecValueNode(fs.toArray)

    def strV(s : String) : Value =
        new CoomaStringValueNode(s)

    def varV(f : FldV) : Value =
        new CoomaVarValueNode(f)

    def vecV(es : Vector[String]) : Value =
        new CoomaVecValueNode(es)

    type FldV = CoomaFldV

    def fldV(f : String, x : String) : FldV =
        new CoomaFldV(f, x)

    def stdout : Writer =
        new PrintWriter(System.out)

    def showTerm(t : Term) : String =
        t.toString

    // Runtime Values

    override type ValueR = RuntimeValue
    override type OutputValueR = org.graalvm.polyglot.Value
    override type Env = Rho
    override type FldR = FieldValueRuntime

    def showRuntimeValue(v : OutputValueR) : String =
        v.toString()

    def strR(str : String) : ValueR =
        new StringRuntimeValue(str)

    def varR(c : String, v : ValueR) : ValueR =
        new VarRuntimeValue(c, v)

    def clsR(f : String, x : String, env : Env, e : Term) : ValueR =
        new FunctionClosure(env, f, x, e)

    def recR(fields : Vector[FldR]) : ValueR =
        new RecRuntimeValue(fields.toArray)

    def vecR(es : Vector[ValueR]) : ValueR =
        new VecRuntimeValue(es)

    def fldR(x : String, v : ValueR) : FldR =
        new FieldValueRuntime(x, v)

    def intR(num : BigInt) : ValueR =
        new IntRuntimeValue(new BigInteger(num.toByteArray))

    def isStrR(value : RuntimeValue) : Option[String] =
        value match {
            case string : StringRuntimeValue => Some(string.getInnerValue)
            case _                           => None
        }

    def isIntR(value : RuntimeValue) : Option[BigInt] =
        value match {
            case int : IntRuntimeValue => Some(int.getInnerValue)
            case _                     => None
        }

    def isRecR(value : RuntimeValue) : Option[Vector[FieldValueRuntime]] =
        value match {
            case rec : RecRuntimeValue => Some(rec.getFields.toVector)
            case _                     => None
        }

    def isVarR(value : ValueR) : Option[(String, ValueR)] =
        value match {
            case varr : VarRuntimeValue => Some((varr.getC(), varr.getV()))
            case _                      => None
        }

    def isVecR(value : ValueR) : Option[Vector[ValueR]] =
        value match {
            case varr : VecRuntimeValue => Some(varr.getVector)
            case _                      => None
        }

    def isFldR(value : FieldValueRuntime) : Option[(String, RuntimeValue)] =
        value match {
            case value : FieldValueRuntime => Some((value.getX, value.getV))
            case _                         => None
        }

    def getFieldName(value : FldR) : String =
        value.getX

    def getFieldValue(value : FldR) : ValueR =
        value.getV

    def emptyEnv : Env = new Rho

    def defineVar(rho : Env, i : String, value : ValueR) : Env =
        rho.extend(i, value)

    def lookupR(rho : Env, x : String) : RuntimeValue = rho.get(x)

    def getConfig : Config = config

}
