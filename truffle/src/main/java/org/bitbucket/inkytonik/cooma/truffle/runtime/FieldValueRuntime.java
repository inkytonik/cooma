package org.bitbucket.inkytonik.cooma.truffle.runtime;


import lombok.Value;

@Value
public class FieldValueRuntime {
    private final String x;
    private final RuntimeValue v;

    @Override
    public String toString() {
        return String.format("%s = %s", x, v.print());
    }
}
