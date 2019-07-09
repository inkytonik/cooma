package org.bitbucket.inkytonik.cooma;

import com.oracle.truffle.api.frame.FrameSlot;
import com.oracle.truffle.api.frame.FrameSlotKind;
import com.oracle.truffle.api.frame.FrameUtil;
import com.oracle.truffle.api.frame.VirtualFrame;
import org.bitbucket.inkytonik.cooma.truffle.CoomaLanguage;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.runtime.CoomaContext;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;
import org.graalvm.polyglot.Value;
import org.bitbucket.inkytonik.cooma.Util;
import java.util.stream.Collectors;

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
