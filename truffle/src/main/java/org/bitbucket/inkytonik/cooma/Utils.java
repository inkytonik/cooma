package org.bitbucket.inkytonik.cooma;

import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.runtime.CoomaContext;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

public class Utils {

    @SuppressWarnings("unchecked")
    public static <T extends RuntimeValue> T  obtainFromRho(CoomaContext coomaContext, String key) {
        return (T) coomaContext.getRho().get(key);
    }

    public static void extendRho(CoomaContext context, String key, RuntimeValue value) {
        replaceRho(context, context.getRho().extend(key, value));
    }

    public static void replaceRho(CoomaContext context, Rho newRho) {
        context.setRho(newRho);
    }


}
