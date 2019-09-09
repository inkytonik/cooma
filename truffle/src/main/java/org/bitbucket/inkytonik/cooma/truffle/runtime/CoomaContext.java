package org.bitbucket.inkytonik.cooma.truffle.runtime;

import com.oracle.truffle.api.TruffleLanguage;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;


/**
 *
 * The run-time state of Cooma during execution.
 * The context is created by the {@link CoomaLanguage}.
 * If two separate scripts run in one Java VM at the same time, they have a different
 * context. Therefore, the context is not a singleton.
 */
public final class CoomaContext {

    private final TruffleLanguage.Env env;
    private Rho rho;
    private String[] applicationArguments;

    public CoomaContext(TruffleLanguage.Env env) {
        this.env = env;
        this.applicationArguments = env.getApplicationArguments();
        rho = new Rho();
    }

    /**
     * Return the current Truffle environment.
     */
    public TruffleLanguage.Env getEnv() {
        return env;
    }

    public Rho getRho() {
        return rho;
    }

    public void setRho(Rho rho) {
        this.rho = rho;
    }

    public String[] getApplicationArguments() {
        return applicationArguments;
    }

    public void setApplicationArguments(String[] applicationArguments) {
        this.applicationArguments = applicationArguments;
    }
}