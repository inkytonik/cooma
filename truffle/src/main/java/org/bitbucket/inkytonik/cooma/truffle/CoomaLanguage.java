package org.bitbucket.inkytonik.cooma.truffle;

import com.oracle.truffle.api.CallTarget;
import com.oracle.truffle.api.CompilerDirectives;
import com.oracle.truffle.api.Truffle;
import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.interop.InteropLibrary;
import com.oracle.truffle.api.interop.TruffleObject;
import com.oracle.truffle.api.interop.UnsupportedMessageException;
import com.oracle.truffle.api.nodes.RootNode;
import org.bitbucket.inkytonik.cooma.truffle.nodes.CoomaRootNode;
import org.bitbucket.inkytonik.cooma.truffle.runtime.*;
import org.bitbucket.inkytonik.cooma.truffle.serialization.CoomaNodeXmlSerializer;


@TruffleLanguage.Registration(id = CoomaLanguage.ID, name = "cooma", defaultMimeType = CoomaLanguage.MIME_TYPE,
        characterMimeTypes = CoomaLanguage.MIME_TYPE, contextPolicy = TruffleLanguage.ContextPolicy.SHARED,
        fileTypeDetectors = CoomaFileDetector.class)
public class CoomaLanguage extends TruffleLanguage<CoomaContext> {

    public static final String ID = "cooma";
    public static final String MIME_TYPE = "application/x-cooma";
    public static final String RHO = "rho";
    public static final String HALT = "$halt";

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
        } else return object instanceof IntRuntimeValue || object instanceof StringRuntimeValue || RuntimeValue.class.isAssignableFrom(object.getClass());
        //TODO: add the rest of the checks as seen in SLLanguage
    }

    @Override
    protected String toString(CoomaContext context, Object value) {
        return toString(value);
    }

    @Override
    protected CallTarget parse(ParsingRequest request) throws Exception {
        String[] args = getCurrentContext(this.getClass()).getEnv().getApplicationArguments();
        RootNode evalMain = new CoomaRootNode(this, CoomaNodeXmlSerializer.fromXML(request.getSource().getCharacters().toString()));
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
            return Type.INT.value;
        } else if (value instanceof StringRuntimeValue) {
            return Type.String.value;
        } else if (value instanceof ErrorRuntimeValue) {
            return Type.Error.value;
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
        INT("Number"),
        String("String"),
        Error("Error");

        final String value;

        Type(java.lang.String msg) {
            this.value = msg;
        }

        public java.lang.String getValue() {
            return value;
        }
    }

}
