package org.bitbucket.inkytonik.cooma.graalvm

import org.bitbucket.inkytonik.cooma.truffle.CoomaLanguage
import org.bitbucket.inkytonik.cooma.truffle.nodes.term._
import org.bitbucket.inkytonik.cooma.truffle.nodes.value._
import org.bitbucket.inkytonik.cooma.truffle.nodes.primitives.{ArgumentP, ConsoleWriteP, ReaderReadP, RowConcatP, RowSelectP}
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue
import org.bitbucket.inkytonik.cooma.truffle.serialization.CoomaNodeXmlSerializer
import org.bitbucket.inkytonik.cooma.{Backend, Config}
import org.bitbucket.inkytonik.kiama.output.PrettyPrinter.{any, layout}
import org.graalvm.polyglot
import org.graalvm.polyglot.Context

trait GraalVMBackend extends Backend {

    // Terms

    override type Term = CoomaTermNode

    override type Value = CoomaValueNode

    def appC(k: String, x: String): CoomaTermNode =
        CoomaAppCTermNodeGen.create(k, x)

    def appF(f: String, k: String, x: String): CoomaTermNode =
        CoomaAppFTermNodeGen.create(f, k, x)

    def letC(k: String, x: String, t: Term, body: Term): CoomaTermNode =
        new CoomaLetCTermNode(k, x, t, body)

    type DefTerm = CoomaDefTerm

    def letF(ds: Vector[DefTerm], body: Term): CoomaTermNode =
        new CoomaLetFTermNode(ds.toArray, body)

    def letV(x: String, v: Value, body: Term): Term =
        new CoomaLetVTermNode(x, v, body)

    def defTerm(f: String, k: String, x: String, body: Term): DefTerm =
        new CoomaDefTerm(f, k, x, body)

    // Values

    def funV(k: String, x: String, body: Term): Value =
        new CoomaFunctionValueNode(k, x, body)

    def intV(i: Int): Value =
        new CoomaIntValueNode(i)

    def prmV(p: Primitive, xs: Vector[String]): Value =
        new CoomaPrimitiveValue(p, xs.toArray)

    def rowV(fs: Vector[FieldValue]): Value =
        new CoomaRowValueNode(fs.toArray)

    def strV(s: String): Value =
        new CoomaStringValueNode(s)

    override type FieldValue = org.bitbucket.inkytonik.cooma.truffle.nodes.value.FieldValue

    def fieldValue(f: String, x: String): FieldValue =
        new FieldValue(f, x)

    /**
      * Custom IR pretty-printer that escapes string terms.
      *
      * @param t
      * @return
      */
    def showTerm(t: Term): String =
    //TODO: print Term properly
        t.toString

    override type Primitive = org.bitbucket.inkytonik.cooma.truffle.nodes.primitives.Primitive

    def argumentP(i: Int): Primitive = {
        new ArgumentP(i)
    }

    def capabilityP(cap: String): Primitive = {
        null
    }

    def consoleWriteP(filename: String): Primitive = {
        new ConsoleWriteP(filename)
    }

    def readerReadP(filename: String): Primitive = {
        new ReaderReadP(filename)
    }

    def rowConcatP(): Primitive = {
        new RowConcatP()
    }

    def rowSelectP(): Primitive = {
        new RowSelectP()
    }

    override type ValueR = RuntimeValue

    def showRuntimeValue(v: ValueR): String = {
        //Runtime value prining is done at a TruffleLanguage level.
        v.toString
    }

    override type Env = Context

    def emptyEnv: Env = {
        Context.newBuilder(CoomaLanguage.ID).build()
    }

    def interpret(term: Term, args: Seq[String], config: Config) = {
        val context = Context.newBuilder(CoomaLanguage.ID)
          .arguments(CoomaLanguage.ID, args.toArray)
          .build()

        val result: polyglot.Value = context.eval(CoomaLanguage.ID, CoomaNodeXmlSerializer.toXML(term))

        if (CoomaLanguage.Type.Error.getValue == result.getMetaObject.toString) {
            config.output().emitln(result)
        }
        if (config.resultPrint()) config.output().emitln(result)

        context.close()
    }

    def repl(env: Env, i: String, printValue: Boolean, config: Config, term: Term): Env = {
        if (config.irPrint())
            config.output().emitln(showTerm(term))
        if (config.irASTPrint())
            config.output().emitln(layout(any(term), 5))

        //val args = config.filenames()

        val result: polyglot.Value = env.eval(CoomaLanguage.ID, CoomaNodeXmlSerializer.toXML(term))

        if (printValue)
            config.output().emitln(s"$i = $result")
        else
            config.output().emitln(i)
        env
    }

}


