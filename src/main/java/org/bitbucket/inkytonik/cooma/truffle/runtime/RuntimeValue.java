package org.bitbucket.inkytonik.cooma.truffle.runtime;

public abstract class RuntimeValue<T> {

    private final T value;

    public RuntimeValue(T value) {
        this.value = value;
    }

    public T getValue() {
        return value;
    }
}
