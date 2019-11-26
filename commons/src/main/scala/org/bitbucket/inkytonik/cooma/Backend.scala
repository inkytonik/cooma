package org.bitbucket.inkytonik.cooma

trait Backend {

    type Term
    def appC(k : String, x : String) : Term
    def appF(f : String, k : String, x : String) : Term
    def casV(x : String, cs : Vector[CaseTerm]) : Term
    def letC(k : String, x : String, t : Term, body : Term) : Term
    def letF(ds : Vector[DefTerm], body : Term) : Term
    def letV(x : String, v : Value, body : Term) : Term

    type CaseTerm
    def caseTerm(c : String, k : String) : CaseTerm

    type DefTerm
    def defTerm(f : String, k : String, x : String, body : Term) : DefTerm

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
    def consoleWriteP(filename : String) : Primitive
    def readerReadP(filename : String) : Primitive
    def recConcatP() : Primitive
    def recSelectP() : Primitive

    def intBinP(op : Primitives.IntPrimBinOp.IntPrimBinOp) : Primitive
    def intRelP(op : Primitives.IntPrimRelOp.IntPrimRelOp) : Primitive

    def showTerm(t : Term) : String

    type ValueR
    def errR(msg : String) : ValueR
    def strR(str : String) : ValueR
    def varR(c : String, v : ValueR) : ValueR
    def intR(num : BigInt) : ValueR
    def clsR(env : Env, f : String, x : String, e : Term) : ValueR
    def recR(fields : Vector[FldR]) : ValueR

    def falseR() : ValueR
    def trueR() : ValueR

    def isErrR(value : ValueR) : Option[String]
    def isStrR(value : ValueR) : Option[String]
    def isIntR(value : ValueR) : Option[BigInt]
    def isRecR(value : ValueR) : Option[Vector[FldR]]

    type FldR
    def fldR(x : String, v : ValueR) : FldR
    def isFldR(value : FldR) : Option[(String, ValueR)]

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
