package org.bitbucket.inkytonik.cooma.truffle.runtime;

import com.oracle.truffle.api.TruffleLanguage;
import org.bitbucket.inkytonik.cooma.truffle.CoomaLanguage;


/**
 *
 * The run-time state of Cooma during execution.
 * The context is created by the {@link CoomaLanguage}.
 * If two separate scripts run in one Java VM at the same time, they have a different
 * context. Therefore, the context is not a singleton.
 */
public final class CoomaContext {

    private final TruffleLanguage.Env env;

    public CoomaContext(TruffleLanguage.Env env) {
        this.env = env;
    }

    /**
     * Return the current Truffle environment.
     */
    public TruffleLanguage.Env getEnv() {
        return env;
    }
}
