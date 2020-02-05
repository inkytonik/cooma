package org.bitbucket.inkytonik.cooma.truffle.nodes;

import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.nodes.Node;
import org.bitbucket.inkytonik.cooma.Utils;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.runtime.CoomaContext;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

public class CoomaNode extends Node {

    protected Rho obtainRho(){
        return getContext().getRho();
    }

    protected RuntimeValue obtainFromRho(String key) {
        return Utils.obtainFromRho(getContext(), key);
    }

    protected void extendRho(String key, RuntimeValue value) {
        Utils.extendRho(getContext(), key, value);
    }

    protected void replaceRho(Rho newRho) {
        Utils.replaceRho(getContext(), newRho);
    }

    protected CoomaContext getContext(){
        return ((CoomaRootNode) getRootNode()).getContext();
    }

    protected String[] getArgs(){
        return getContext().getApplicationArguments();
    }
}
