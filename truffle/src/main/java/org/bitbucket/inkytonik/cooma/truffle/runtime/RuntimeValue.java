package org.bitbucket.inkytonik.cooma.truffle.runtime;

import com.oracle.truffle.api.interop.TruffleObject;
import de.uka.ilkd.pp.*;

public abstract class RuntimeValue implements TruffleObject, PrettyPrintable {

    public Boolean isHostObject() {
        return true;
    }

    public String print() {
        return this.toString();
    }

    public abstract <Exc extends java.lang.Exception> void prettyPrint(DataLayouter<Exc> l) throws Exc;

    @SuppressWarnings("unchecked")
    public String pprint() {
        try {
            StringBackend backend = new StringBackend(80);
            DataLayouter layouter = new DataLayouter(backend, 4);
            layouter.print(this);
            layouter.close();
            return backend.getString();
        } catch (Exception e) {
            e.printStackTrace();
            return e.getMessage();
        }
    }
}
