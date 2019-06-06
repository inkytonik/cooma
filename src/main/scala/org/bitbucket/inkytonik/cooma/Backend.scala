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

trait Backend {

    type Term
    def appC(k : String, x : String) : Term
    def appF(f : String, k : String, x : String) : Term
    def letC(k : String, x : String, t : Term, body : Term) : Term
    def letF(ds : Vector[DefTerm], body : Term) : Term
    def letV(x : String, v : Value, body : Term) : Term

    type DefTerm
    def defTerm(f : String, k : String, x : String, body : Term) : DefTerm

    type Value
    def funV(k : String, x : String, body : Term) : Value
    def intV(i : Int) : Value
    def prmV(p : Primitive, xs : Vector[String]) : Value
    def rowV(fs : Vector[FieldValue]) : Value
    def strV(s : String) : Value

    type FieldValue
    def fieldValue(f : String, x : String) : FieldValue

    type Primitive
    def argumentP(i : Int) : Primitive
    def capabilityP(cap : String) : Primitive
    def consoleWriteP(filename : String) : Primitive
    def readerReadP(filename : String) : Primitive
    def rowConcatP() : Primitive
    def rowSelectP() : Primitive

    def showTerm(t : Term) : String

    type ValueR
    def showRuntimeValue(v : ValueR) : String

    type Env
    def emptyEnv : Env
    def consEnv(env : Env, i : String, v : ValueR) : Env

    def interpret(term : Term, args : Seq[String], config : Config)
    def interpret(term : Term, env : Env, args : Seq[String]) : ValueR

}
