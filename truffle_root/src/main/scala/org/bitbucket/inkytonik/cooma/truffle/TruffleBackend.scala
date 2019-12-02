package org.bitbucket.inkytonik.cooma.truffle

import java.io.PrintWriter
import java.math.BigInteger

import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho
import org.bitbucket.inkytonik.cooma.truffle.runtime._
import org.bitbucket.inkytonik.cooma.{Backend, Config, Primitives}

class TruffleBackend(config : Config) extends Backend {

    import org.bitbucket.inkytonik.cooma.truffle.nodes.term._
    import org.bitbucket.inkytonik.cooma.truffle.nodes.value._
    import scala.math.BigInt;

    override def backendName : String = "Graal"

    // Terms
    override type Term = CoomaTermNode

    override type Value = CoomaValueNode

    def appC(k : String, x : String) : CoomaTermNode = CoomaAppCTermNodeGen.create(k, x)

    def appF(f : String, k : String, x : String) : CoomaTermNode = CoomaAppFTermNodeGen.create(f, k, x)

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

    // Values

    def funV(k : String, x : String, body : Term) : Value =
        new CoomaFunctionValueNode(k, x, body)

    def intV(i : BigInt) : Value =
        new CoomaIntValueNode(i.bigInteger)

    def prmV(p : Primitive, xs : Vector[String]) : Value =
        new CoomaPrimitiveValue(this, p, xs.toArray)

    def recV(fs : Vector[FieldValue]) : Value =
        new CoomaRecValueNode(fs.toArray)

    def strV(s : String) : Value =
        new CoomaStringValueNode(s)

    def varV(c : String, x : String) : Value =
        new CoomaVarValueNode(c, x)

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

    type Primitive = org.bitbucket.inkytonik.cooma.Primitives.Primitive[TruffleBackend]

    def argumentP(i : Int) : Primitive =
        Primitives.ArgumentP(i)

    def capabilityP(cap : String) : Primitive =
        Primitives.CapabilityP(cap)

    def writerWriteP(filename : String) : Primitive =
        Primitives.WriterWriteP(filename, new PrintWriter(System.out))

    def readerReadP(filename : String) : Primitive =
        Primitives.ReaderReadP(filename)

    def recConcatP() : Primitive =
        Primitives.RecConcatP()

    def recSelectP() : Primitive =
        Primitives.RecSelectP()

    def intBinP(op : Primitives.IntPrimBinOp.IntPrimBinOp) : Primitive =
        Primitives.IntBinOpP(op)

    def intRelP(op : Primitives.IntPrimRelOp.IntPrimRelOp) : Primitive =
        Primitives.IntRelOp(op)

    def stringP(op : Primitives.StrPrimOp.StrPrimOp) : Primitive =
        Primitives.StringPrimitive(op)

    //Runtime Values

    override type ValueR = RuntimeValue[_]
    override type OutputValueR = org.graalvm.polyglot.Value
    override type Env = Rho
    override type FldR = FieldValueRuntime

    def showRuntimeValue(v : OutputValueR) : String = {
        v.toString
    }

    def errR(msg : String) : ValueR =
        new ErrorRuntimeValue(msg)

    def strR(str : String) : ValueR =
        new StringRuntimeValue(str)

    def varR(c : String, v : ValueR) : ValueR =
        new VarRuntimeValue(c, v)

    val unit = new RecRuntimeValue(Array.empty[FieldValueRuntime])
    def falseR = new VarRuntimeValue("false", unit)
    def trueR = new VarRuntimeValue("true", unit)

    def clsR(env : Env, f : String, x : String, e : Term) : ValueR =
        new FunctionClosure(env, f, x, e)

    def recR(fields : Vector[FldR]) : ValueR =
        new RecRuntimeValue(fields.toArray)

    def fldR(x : String, v : ValueR) : FldR =
        new FieldValueRuntime(x, v)

    def intR(num : BigInt) : ValueR =
        new IntRuntimeValue(new BigInteger(num.toByteArray))

    def isErrR(value : RuntimeValue[_]) : Option[String] =
        value match {
            case error : ErrorRuntimeValue => Some(error.getMessage)
            case _                         => None
        }

    def isStrR(value : RuntimeValue[_]) : Option[String] =
        value match {
            case string : StringRuntimeValue => Some(string.getInnerValue)
            case _                           => None
        }

    def isIntR(value : RuntimeValue[_]) : Option[BigInt] =
        value match {
            case int : IntRuntimeValue => Some(int.getInnerValue)
            case _                     => None
        }

    override def isRecR(value : RuntimeValue[_]) : Option[Vector[FieldValueRuntime]] =
        value match {
            case rec : RecRuntimeValue => Some(rec.getFields.toVector)
            case _                     => None
        }

    override def isFldR(value : FieldValueRuntime) : Option[(String, RuntimeValue[_])] =
        value match {
            case value : FieldValueRuntime => Some((value.getX, value.getV))
            case _                         => None
        }

    override def emptyEnv : Rho = new Rho

    override def lookupR(rho : Rho, x : String) : RuntimeValue[_] = rho.get(x)

    override def getConfig : Config = config

}
