package org.bitbucket.inkytonik.cooma.truffle.runtime;

import com.oracle.truffle.api.interop.TruffleObject;
import de.uka.ilkd.pp.DataLayouter;
import lombok.EqualsAndHashCode;
import lombok.Value;
import lombok.val;

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
        return this.pprint();
    }

    @Override
    public <Exc extends java.lang.Exception> void prettyPrint(DataLayouter<Exc> l) throws Exc {
        if (fields.length == 0)
            l.print("{}");
        else {
            l.beginCInd(0);
            l.beginCInd().print("{").brk(1, 0);
            for (int i = 0; i < fields.length; i++) {
                l.print(fields[i]);
                if (i != fields.length - 1)
                    l.print(",").brk(1, 0);
            }
            l.end().brk(1, 0).print("}");
            l.end();
        }
    }

    public static RecRuntimeValue empty() {
        return new RecRuntimeValue(new FieldValueRuntime[] {});
    }

}
