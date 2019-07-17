package org.bitbucket.inkytonik.cooma.truffle.runtime;

import com.oracle.truffle.api.interop.TruffleObject;

public abstract class RuntimeValue<T extends RuntimeValue<T>> implements Comparable<T>, TruffleObject  {

    public RuntimeValue<T> getValue(){
        return this;
    }


    public abstract String print();
}
