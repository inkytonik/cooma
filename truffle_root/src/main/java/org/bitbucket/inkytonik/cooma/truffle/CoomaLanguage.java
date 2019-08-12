package org.bitbucket.inkytonik.cooma.truffle;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.RootNode;
import org.bitbucket.inkytonik.cooma.Config;
import org.bitbucket.inkytonik.cooma.CoomaParserSyntax;
import org.bitbucket.inkytonik.cooma.Driver;
import org.bitbucket.inkytonik.cooma.REPL;
import org.bitbucket.inkytonik.cooma.truffle.nodes.CoomaRootNode;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaTermNode;
import org.bitbucket.inkytonik.cooma.truffle.runtime.*;
import org.bitbucket.inkytonik.cooma.truffle.scala.GraalVMCompiler;
import org.bitbucket.inkytonik.kiama.util.Source;

import java.util.Arrays;


@TruffleLanguage.Registration(id = CoomaConstants.ID, name = "cooma", defaultMimeType = CoomaConstants.MIME_TYPE,
        characterMimeTypes = CoomaConstants.MIME_TYPE, contextPolicy = TruffleLanguage.ContextPolicy.SHARED,
        fileTypeDetectors = CoomaFileDetector.class, interactive = true)
public class CoomaLanguage extends TruffleLanguage<CoomaContext> {



    public static String toString(Object value) {
        try {
            if (value == null) {
                return "ANY";
            }
            InteropLibrary interop = InteropLibrary.getFactory().getUncached(value);
            if (interop.fitsInLong(value)) {
                return Long.toString(interop.asLong(value));
            } else if (interop.isBoolean(value)) {
                return Boolean.toString(interop.asBoolean(value));
            } else if (interop.isString(value)) {
                return interop.asString(value);
            } else if (interop.isNull(value)) {
                return "NULL";
            } else if (interop.hasMembers(value)) {
                return "Object";
            } else if (value instanceof RuntimeValue) {
                return ((RuntimeValue) value).print();
            } else {
                return "Unsupported";
            }
        } catch (UnsupportedMessageException e) {
            CompilerDirectives.transferToInterpreter();
            throw new AssertionError();
        }
    }

    @Override
    protected CoomaContext createContext(TruffleLanguage.Env env) {
        return new CoomaContext(env);
    }

    @Override
    protected boolean isObjectOfLanguage(Object object) {
        if (!(object instanceof TruffleObject)) {
            return false;
        } else return RuntimeValue.class.isAssignableFrom(object.getClass());
    }

    @Override
    protected String toString(CoomaContext context, Object value) {
        return toString(value);
    }

    @Override
    protected CallTarget parse(ParsingRequest request) throws Exception {
        String[] args = getCurrentContext(this.getClass()).getEnv().getApplicationArguments();
        Config config = new Config(scala.collection.JavaConverters.collectionAsScalaIterableConverter(Arrays.asList(args)).asScala().toSeq());
        config.verify();
        final CoomaTermNode[] main = new CoomaTermNode[1];

        Driver driver = new Driver(){

            @Override
            public REPL createREPL(Config config) {
                return TruffleRepl.repl(config);
            }

            @Override
            public void process(Source source, CoomaParserSyntax.Program prog, Config config) {
                GraalVMCompiler compiler = new GraalVMCompiler(config);
                main[0] = compiler.compileCommand(prog);
                if (config.irPrint().isDefined())
                    config.output().apply().emitln(compiler.showTerm(main[0]));
                if (config.irASTPrint().isDefined())
                    config.output().apply().emitln(
                            JavaCoomaParserPrettyPrinter.getInstance().layout
                            (JavaCoomaParserPrettyPrinter.getInstance().any( main[0]), 5));
            }
        };

        driver.run(config);

        RootNode evalMain = new CoomaRootNode(this, main[0]);
        return Truffle.getRuntime().createCallTarget(evalMain);
    }


    @Override
    protected Object findMetaObject(CoomaContext context, Object value) {
        return getMetaObject(value);
    }

    public static String getMetaObject(Object value) {
        if (value == null) {
            return "ANY";
        }
        InteropLibrary interop = InteropLibrary.getFactory().getUncached(value);
        if ( value instanceof IntRuntimeValue || interop.isNumber(value)) {
            return Type.Int.value;
        } else if (value instanceof StringRuntimeValue) {
            return Type.String.value;
        } else if (value instanceof ErrorRuntimeValue) {
            return Type.Error.value;
        } else if (value instanceof ContinuationClosure) {
            return Type.Closure.value;
        } else if (value instanceof RowRuntimeValue) {
            return Type.Row.value;
        } else if (interop.isNull(value)) {
            return "NULL";
        } else if (interop.isExecutable(value)) {
            return "Function";
        } else if (interop.hasMembers(value)) {
            return "Object";
        } else {
            return "Unsupported";
        }
    }

    public enum Type{
        Int("Number"),
        String("String"),
        Error("Error"),
        Row("Row"),
        Closure("Closure");

        final String value;

        Type(java.lang.String msg) {
            this.value = msg;
        }

        public java.lang.String getValue() {
            return value;
        }
    }

}
