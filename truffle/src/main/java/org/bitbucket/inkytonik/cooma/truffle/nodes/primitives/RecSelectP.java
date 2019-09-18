package org.bitbucket.inkytonik.cooma.truffle.nodes.primitives;

import org.bitbucket.inkytonik.cooma.truffle.exceptions.CoomaException;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.runtime.ErrorRuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.FieldValueRuntime;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RecRuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

import java.util.Arrays;
import java.util.Optional;

public class RecSelectP extends Primitive {

    @Override
    public Integer getNumargs() {
        return 2;
    }

    @Override
    public RuntimeValue run(Rho rho, String[] xs, String[] args) {

        String recId = xs[0];
        String fieldId = xs[1];

        RuntimeValue rec = rho.get(recId);
        if (rec instanceof RecRuntimeValue){
            Optional<FieldValueRuntime> opV = Arrays
                    .stream(((RecRuntimeValue) rec).getFields())
                    .filter( (FieldValueRuntime fieldR) -> fieldR.getX().equals(fieldId) )
                    .findFirst();

            if (opV.isPresent()){
                return opV.get().getV();
            }else{
                throw new CoomaException(String.format("%s: can't find field %s in %s", getShow(), fieldId, Arrays.toString(((RecRuntimeValue) rec).getFields())), this);
            }
        }else if (rec instanceof ErrorRuntimeValue){
            return rec;
        }
        throw new CoomaException(String.format("%s: %s is %s, looking for field %s", getShow(), recId, rec, fieldId ), this);
    }

    @Override
    public String getShow() {
        return "select";
    }
}
