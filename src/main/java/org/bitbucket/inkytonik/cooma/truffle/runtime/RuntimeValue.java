package org.bitbucket.inkytonik.cooma.truffle.runtime;

public abstract class RuntimeValue  {

    public RuntimeValue getValue(){
        return this;
    }

    public abstract String print();
}
