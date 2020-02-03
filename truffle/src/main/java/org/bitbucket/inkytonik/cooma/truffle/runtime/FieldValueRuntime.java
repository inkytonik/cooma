package org.bitbucket.inkytonik.cooma.truffle.runtime;

import de.uka.ilkd.pp.DataLayouter;
import de.uka.ilkd.pp.PrettyPrintable;
import lombok.Value;
import java.util.Comparator;

@Value
public class FieldValueRuntime implements Comparable<FieldValueRuntime>, PrettyPrintable{
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

    @Override
    public <Exc extends java.lang.Exception> void prettyPrint(DataLayouter<Exc> l) throws Exc {
        l.print(x).print(" = ").print(v);
    }

}
