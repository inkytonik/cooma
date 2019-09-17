package org.bitbucket.inkytonik.cooma.truffle

import org.bitbucket.inkytonik.cooma.truffle.nodes.primitives._
import org.bitbucket.inkytonik.cooma.truffle.nodes.term._
import org.bitbucket.inkytonik.cooma.truffle.nodes.value._
import org.bitbucket.inkytonik.cooma.{Backend, Config}
import org.graalvm.polyglot.Context

class TruffleBackend(config : Config) extends Backend {

    import org.bitbucket.inkytonik.cooma.CoomaParserSyntax.Expression;
    import scala.math.BigInt;

    override def backendName : String = "Graal"

    // Terms
    override type Term = CoomaTermNode

    override type Value = CoomaValueNode

    def appC(k : String, x : String) : CoomaTermNode = CoomaAppCTermNodeGen.create(k, x)

    def appF(f : String, k : String, x : String) : CoomaTermNode = CoomaAppFTermNodeGen.create(f, k, x)

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

    def intV(i : BigInt) : Value =
        new CoomaIntValueNode(i.bigInteger)

    def prmV(p : Primitive, xs : Vector[String]) : Value =
        new CoomaPrimitiveValue(p, xs.toArray)

    def rowV(fs : Vector[FieldValue]) : Value =
        new CoomaRowValueNode(fs.toArray)

    def strV(s : String) : Value =
        new CoomaStringValueNode(s)

    override type FieldValue = org.bitbucket.inkytonik.cooma.truffle.nodes.value.FieldValue

    def fieldValue(f : String, x : String) : FieldValue =
        new FieldValue(f, x)

    /**
     * Custom IR pretty-printer that escapes string terms.
     *
     * @param t
     * @return
     */
    def showTerm(t : Term) : String =
        t.toString

    override type Primitive = org.bitbucket.inkytonik.cooma.truffle.nodes.primitives.Primitive

    def argumentP(i : Int) : Primitive = {
        new ArgumentP(i)
    }

    def capabilityP(cap : String) : Primitive = {
        new CapabilityP(cap)
    }

    def consoleWriteP(filename : String) : Primitive = {
        new WriterWriteP(filename)
    }

    def readerReadP(filename : String) : Primitive = {
        new ReaderReadP(filename)
    }

    def rowConcatP() : Primitive = {
        new RowConcatP()
    }

    def rowSelectP() : Primitive = {
        new RowSelectP()
    }

    override type ValueR = org.graalvm.polyglot.Value

    def showRuntimeValue(v : ValueR) : String = {
        v.toString
    }

    override type Env = Context

    def emptyEnv : Env = {
        Context.newBuilder(CoomaConstants.ID).build()
    }

    def repl(
        env : Env, i : String, tipe : Expression,
        config : Config, term : Term
    ) : Env = {
        env
    }

}
