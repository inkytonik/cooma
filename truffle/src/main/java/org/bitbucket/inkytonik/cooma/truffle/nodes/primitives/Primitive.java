package org.bitbucket.inkytonik.cooma.truffle.nodes.primitives;

import org.bitbucket.inkytonik.cooma.truffle.exceptions.CoomaException;
import org.bitbucket.inkytonik.cooma.truffle.nodes.CoomaNode;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

public abstract class Primitive extends CoomaNode {

    public abstract Integer getNumargs();

    public RuntimeValue eval(Rho rho, String[] xs, String[] args) {
        if (xs.length == getNumargs()){
            return run(rho, xs, args);
        }
        throw new CoomaException(String.format("%s: expected %s arg(s), got %s", getShow(), getNumargs(), xs.length ), this);
    }

    public abstract RuntimeValue run(Rho rho, String[] xs, String[] args);

    public abstract String getShow();

}
