package org.bitbucket.inkytonik.cooma.truffle.runtime;

import com.oracle.truffle.api.interop.TruffleObject;
import lombok.EqualsAndHashCode;
import lombok.Value;
import lombok.val;

import java.util.Arrays;


@Value
@EqualsAndHashCode(callSuper=false)
public class RecRuntimeValue extends RuntimeValue<RecRuntimeValue> implements TruffleObject {

    private final FieldValueRuntime[] fields;

    public RecRuntimeValue(FieldValueRuntime[] fields) {
        this.fields = fields;
    }

    @Override
    public int compareTo(RecRuntimeValue recRuntimeValue) {

        if (this.getFields().length == recRuntimeValue.getFields().length){
            for (int i = 0; i < this.getFields().length ; i++) {
                FieldValueRuntime local = this.getFields()[i];
                FieldValueRuntime theirs = recRuntimeValue.getFields()[i];
                if (local.compareTo(theirs) != 0) {
                    return -1;
                }
            }
            return 0;
        } else{
            return -1;
        }

    }


    @Override
    public String toString() {
        val builder = new StringBuilder();
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
        return String.format("{%s}", this.toString()) ;
    }

    public static RecRuntimeValue empty(){
        return new RecRuntimeValue(new FieldValueRuntime[]{});
    }
}
