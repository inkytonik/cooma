package org.bitbucket.inkytonik.cooma.truffle.runtime;

public abstract class FuntionClosureHolder extends RuntimeValue{
    public abstract FunctionClosure get(String key);
}
