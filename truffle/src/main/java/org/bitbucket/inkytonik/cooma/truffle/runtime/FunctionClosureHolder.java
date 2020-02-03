package org.bitbucket.inkytonik.cooma.truffle.runtime;

public abstract class FunctionClosureHolder extends RuntimeValue{
    public abstract FunctionClosure get(String key);

    @Override
    public String print() {
        return "<function>";
    }

}
