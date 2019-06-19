package org.bitbucket.inkytonik.cooma.truffle.runtime;

import com.oracle.truffle.api.interop.TruffleObject;
import lombok.Value;


@Value
public class RowRuntimeValue extends RuntimeValue implements TruffleObject, Comparable<RowRuntimeValue> {

    private final FieldValueRuntime[] fields;

    public RowRuntimeValue(FieldValueRuntime[] fields) {
        this.fields = fields;
    }

    @Override
    public int compareTo(RowRuntimeValue rowRuntimeValue) {
        //TODO: fixme
        return 0;
    }


    @Override
    public String toString() {
        StringBuilder builder = new StringBuilder();
        for (int i = 0; i < fields.length; i++) {
            builder.append(fields[i].toString());
            if (i != fields.length - 1) {
                builder.append(", ");
            }
        }
        return builder.toString();
    }

    @Override
    public String print() {
        //text("{") <> ssep(v1.map(toDocField), text(",") <> space) <> text("}")
        return String.format("{%s}", this.toString()) ;
    }
}
