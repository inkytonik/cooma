package org.bitbucket.inkytonik.cooma.truffle.runtime;

import com.oracle.truffle.api.interop.TruffleObject;

public abstract class RuntimeValue<T extends RuntimeValue<T>> implements Comparable<T>, TruffleObject  {

    public T getValue(){
        return (T) this;
    }


    public abstract String print();
}
