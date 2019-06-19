package org.bitbucket.inkytonik.cooma.truffle.primitives;

import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

public abstract class Primitive {

    public abstract Integer getNumargs();

    public RuntimeValue eval(Rho rho, String[] xs) throws Exception {
        if (xs.length == getNumargs()){
            return run(rho, xs);
        }
        throw new Exception(String.format("%s: expected %s arg(s), got %s", getShow(), getNumargs(), xs.length ));
    }

    public abstract RuntimeValue run(Rho rho, String[] xs) throws Exception;

    public abstract String getShow();

}
