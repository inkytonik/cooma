package org.bitbucket.inkytonik.cooma.truffle.nodes.primitives;

import org.apache.commons.lang3.ArrayUtils;
import org.bitbucket.inkytonik.cooma.truffle.exceptions.CoomaException;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RowRuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

public class RowConcatP extends Primitive {
    @Override
    public Integer getNumargs() {
        return 2;
    }

    @Override
    public RuntimeValue run(Rho rho, String[] xs, String[] args) {

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
                throw new CoomaException(String.format("%s: left argument %s of & is non-row %s", getShow(), right, rowR.print()), this);
            }

        } else {
            throw new CoomaException(String.format("%s: left argument %s of & is non-row %s", getShow(), left, rowl.print()), this);
        }
    }

    @Override
    public String getShow() {
        return "concat";
    }
}