package org.bitbucket.inkytonik.cooma.truffle.nodes;

import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.nodes.Node;
import org.bitbucket.inkytonik.cooma.Utils;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.runtime.CoomaContext;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

public class CoomaNode extends Node {

    protected Rho obtainRho(){
        return getContext().get().getRho();
    }

    @SuppressWarnings("unchecked")
    protected <T extends RuntimeValue> T  obtainFromRho(String key) {
        return Utils.obtainFromRho(getContext().get(), key);
    }

    protected void extendRho(String key, RuntimeValue value) {
        Utils.extendRho(getContext().get(), key, value);
    }

    protected void replaceRho(Rho newRho) {
        Utils.replaceRho(getContext().get(), newRho);
    }

    protected TruffleLanguage.ContextReference<CoomaContext> getContext(){
        return ((CoomaRootNode) getRootNode()).getContext();
    }

    protected String[] getArgs(){
        return getContext().get().getEnv().getApplicationArguments();
    }
}
