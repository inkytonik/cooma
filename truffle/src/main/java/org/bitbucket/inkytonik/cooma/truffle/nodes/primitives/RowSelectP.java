package org.bitbucket.inkytonik.cooma.truffle.nodes.primitives;

import org.bitbucket.inkytonik.cooma.truffle.CoomaException;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.runtime.ErrorRuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.FieldValueRuntime;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RowRuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

import java.util.Arrays;
import java.util.Optional;

public class RowSelectP extends Primitive {

    @Override
    public Integer getNumargs() {
        return 2;
    }

    @Override
    public RuntimeValue run(Rho rho, String[] xs, String[] args) {

        String rowId = xs[0];
        String fieldId = xs[1];

        RuntimeValue row = rho.get(rowId);
        if (row instanceof RowRuntimeValue){
            Optional<FieldValueRuntime> opV = Arrays
                    .stream(((RowRuntimeValue) row).getFields())
                    .filter( (FieldValueRuntime fieldR) -> fieldR.getX().equals(fieldId) )
                    .findFirst();

            if (opV.isPresent()){
                return opV.get().getV();
            }else{
                throw new CoomaException(String.format("%s: can't find field %s in %s", getShow(), fieldId, Arrays.toString(((RowRuntimeValue) row).getFields())), this);
            }
        }else if (row instanceof ErrorRuntimeValue){
            return row;
        }
        throw new CoomaException(String.format("%s: %s is %s, looking for field %s", getShow(), rowId, row, fieldId ), this);
    }

    @Override
    public String getShow() {
        return "select";
    }
}

