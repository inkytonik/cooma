package org.bitbucket.inkytonik.cooma.graalvm

import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho
import org.bitbucket.inkytonik.cooma.truffle.nodes.term._
import org.bitbucket.inkytonik.cooma.truffle.nodes.value._
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue
import org.bitbucket.inkytonik.cooma.truffle.serialization.CoomaNodeXmlSerializer
import org.bitbucket.inkytonik.cooma.{Backend, Config}
import org.graalvm.polyglot
import org.graalvm.polyglot.Context

trait GraalVMBackend extends Backend {

    // Terms

    override type Term = CoomaTermNode

    override type Value = CoomaValueNode

    def appC(k : String, x : String) : CoomaTermNode =
        CoomaAppCTermNodeGen.create(k, x)

    def appF(f : String, k : String, x : String) : CoomaTermNode =
        CoomaAppFTermNodeGen.create(f, k, x)

    def letC(k : String, x : String, t : Term, body : Term) : CoomaTermNode =
        new CoomaLetCTermNode(k, x, t, body)

    type DefTerm = CoomaDefTerm

    def letF(ds : Vector[DefTerm], body : Term) : CoomaTermNode =
        new CoomaLetFTermNode(ds.toArray, body)

    def letV(x : String, v : Value, body : Term) : Term =
        new CoomaLetVTermNode(x, v, body)

    def defTerm(f : String, k : String, x : String, body : Term) : DefTerm =
        new CoomaDefTerm(f, k, x, body)

    // Values

    def funV(k : String, x : String, body : Term) : Value =
        new CoomaFunctionValueNode(k, x, body)

    def intV(i : Int) : Value =
        new CoomaIntValueNode(i)

    def prmV(p : Primitive, xs : Vector[String]) : Value =
        //TODO: complete this
        null

    def rowV(fs : Vector[FieldValue]) : Value =
        //TODO: complete this
        null

    def strV(s : String) : Value =
        new CoomaStringValueNode(s)

    override type FieldValue = Value

    def fieldValue(f : String, x : String) : FieldValue =
        //TODO: complete this
        //FieldValue(f, x)
        null

    /**
     * Custom IR pretty-printer that escapes string terms.
     *
     * @param t
     * @return
     */
    def showTerm(t : Term) : String =
        t.toString

    override type Primitive = Null

    def argumentP(i : Int) : Primitive = {
        null
    }

    def capabilityP(cap : String) : Primitive = {
        null
    }

    def consoleWriteP(filename : String) : Primitive = {
        null
    }

    def readerReadP(filename : String) : Primitive = {
        null
    }

    def rowConcatP() : Primitive = {
        null
    }

    def rowSelectP() : Primitive = {
        null
    }

    override type ValueR = RuntimeValue

    def showRuntimeValue(v : ValueR) : String = {
        //TODO: show better runtime value representations.
        v.toString
    }

    override type Env = Rho

    def emptyEnv : Env = {
        new Rho()
    }

    def consEnv(env : Env, i : String, v : ValueR) : Env = {
        env.extend(i, v)
    }

    def interpret(term : Term, args : Seq[String], config : Config) = {
        val context = Context.newBuilder("cooma").build()

        val result: polyglot.Value = context.eval("cooma", CoomaNodeXmlSerializer.toXML(term))

        if (config.resultPrint()) config.output().emitln(result)

        context.close()
    }

    def interpret(term : Term, env : Env, args : Seq[String]) : ValueR = {
        //val evalMain = new CoomaRootNode(new CoomaLanguage(), term)
        null
    }
}

