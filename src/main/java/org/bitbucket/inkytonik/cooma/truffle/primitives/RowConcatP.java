package org.bitbucket.inkytonik.cooma.truffle.primitives;

import org.apache.commons.lang3.ArrayUtils;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RowRuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

public class RowConcatP extends Primitive {
    @Override
    public Integer getNumargs() {
        return 2;
    }

    @Override
    public RuntimeValue run(Rho rho, String[] xs) throws Exception {

        String left = xs[0];
        String right = xs[1];

        RuntimeValue rowl = rho.get(left);
        if (rowl instanceof RowRuntimeValue) {
            RowRuntimeValue rowlL = (RowRuntimeValue) rowl;

            RuntimeValue rowR = rho.get(right);
            if (rowR instanceof RowRuntimeValue) {
                RowRuntimeValue rowlR = (RowRuntimeValue) rowR;
                return new RowRuntimeValue(ArrayUtils.addAll(rowlL.getFields(),rowlR.getFields()));
            } else {
                //TODO: Fix this kind of exception
                throw new Exception(String.format("%s: left argument %s of & is non-row %s", getShow(), right, rowR.print()));
            }

        } else {
            //TODO: Fix this kind of exception
            throw new Exception(String.format("%s: left argument %s of & is non-row %s", getShow(), left, rowl.print()));
        }

    }

    @Override
    public String getShow() {
        return "concat";
    }
}