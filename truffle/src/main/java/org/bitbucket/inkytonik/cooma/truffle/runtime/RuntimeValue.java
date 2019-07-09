package org.bitbucket.inkytonik.cooma.truffle.runtime;

import com.oracle.truffle.api.interop.TruffleObject;

public abstract class RuntimeValue implements TruffleObject {

    public RuntimeValue getValue(){
        return this;
    }

    public abstract String print();
}
