package org.bitbucket.inkytonik.cooma.truffle.nodes.primitives;

import lombok.EqualsAndHashCode;
import lombok.Value;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.runtime.ErrorRuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.StringRuntimeValue;

@Value
@EqualsAndHashCode(callSuper = true)
public class ArgumentP extends Primitive {

    int index;

    @Override
    public Integer getNumargs() {
        return 0;
    }

    @Override
    public RuntimeValue run(Rho rho, String[] xs, String[] args) {

        if (getIndex() < 0 || getIndex() >= args.length){
            return new ErrorRuntimeValue(String.format("command-line argument %d does not exist (arg count = %d)",getIndex(), args.length));
        }

        return new StringRuntimeValue(String.valueOf(args[getIndex()]));
    }

    @Override
    public String getShow() {
        return String.format("arg %d",getIndex());
    }

}
