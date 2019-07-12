package org.bitbucket.inkytonik.cooma.truffle.runtime;


import lombok.Value;

import java.util.Comparator;

@Value
public class FieldValueRuntime implements Comparable<FieldValueRuntime> {
    private final String x;
    private final RuntimeValue v;

    @Override
    public String toString() {
        return String.format("%s = %s", x, v.print());
    }

    @Override
    public int compareTo(FieldValueRuntime fieldValueRuntime) {
        return Comparator.comparing(FieldValueRuntime::getX)
                .thenComparing(FieldValueRuntime::getV)
                .compare(this, fieldValueRuntime);
    }
}
