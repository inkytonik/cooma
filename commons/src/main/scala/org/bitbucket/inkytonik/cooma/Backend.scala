package org.bitbucket.inkytonik.cooma

trait Backend {

    import org.bitbucket.inkytonik.cooma.CoomaParserSyntax.Type

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
    def intV(i : BigInt) : Value
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

    def repl(
        env : Env, i : String, tipe : Type,
        config : Config, term : Term
    ) : Env

    def backendName : String

}
