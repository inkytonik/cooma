package org.bitbucket.inkytonik.cooma.truffle.runtime;

public abstract class FunctionClosureHolder<T extends FunctionClosureHolder<T>> extends RuntimeValue<T>{
    public abstract FunctionClosure get(String key);

    @Override
    public String print() {
        return "<function>";
    }

}
