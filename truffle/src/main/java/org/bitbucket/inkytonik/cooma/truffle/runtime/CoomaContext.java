package org.bitbucket.inkytonik.cooma.truffle.runtime;

import com.oracle.truffle.api.TruffleLanguage;
import lombok.val;
import org.bitbucket.inkytonik.cooma.Backend;
import org.bitbucket.inkytonik.cooma.Config;
import org.bitbucket.inkytonik.cooma.Primitives;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;

import java.io.PrintStream;
import java.util.Arrays;

import static scala.collection.JavaConverters.collectionAsScalaIterableConverter;


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
    private PrintStream originalSout;
    private Backend truffleBackend;
    private Config config;


    public CoomaContext(TruffleLanguage.Env env, Backend truffleBackend, Config config) {
        this.env = env;
        this.applicationArguments = env.getApplicationArguments();
        this.originalSout = System.out;
        this.truffleBackend = truffleBackend;
        this.config = config;
        this.config.verify();
        System.setOut(new PrintStream(env.out()));
        this.rho = predefRho();
    }

    private Rho predefRho() {
        val rho = new Rho();
        RecRuntimeValue unit = new RecRuntimeValue(new FieldValueRuntime[] {});
        Rho extendedRho =  rho.extend("false", new VarRuntimeValue("false", unit))
                    .extend("true", new VarRuntimeValue("true", unit));

        extendedRho = extendedRho.extend("Ints", (RuntimeValue) Primitives.generateDynamicRuntime(truffleBackend));
        return extendedRho;
    }

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

    public PrintStream getOriginalSout() {
        return originalSout;
    }

    public Config getConfig() {
        return config;
    }

    public Backend getTruffleBackend() {
        return truffleBackend;
    }
}
