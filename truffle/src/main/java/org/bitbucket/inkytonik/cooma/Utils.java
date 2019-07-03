package org.bitbucket.inkytonik.cooma;

import com.oracle.truffle.api.frame.FrameSlot;
import com.oracle.truffle.api.frame.FrameSlotKind;
import com.oracle.truffle.api.frame.FrameUtil;
import com.oracle.truffle.api.frame.VirtualFrame;
import org.bitbucket.inkytonik.cooma.truffle.CoomaLanguage;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;
import org.graalvm.polyglot.Value;

import java.util.stream.Collectors;

public class Utils {

    public static  FrameSlot obtainRhoFrameSlot(VirtualFrame frame) {
        return frame.getFrameDescriptor().findOrAddFrameSlot(CoomaLanguage.RHO, null, FrameSlotKind.Object);
    }

    public static  Rho obtainRhoFromFrame(VirtualFrame frame) {
        return (Rho) FrameUtil.getObjectSafe(frame, obtainRhoFrameSlot(frame));
    }

    @SuppressWarnings("unchecked")
    public static <T extends RuntimeValue> T  obtainFromRho(VirtualFrame frame, String key) {
        return (T) obtainRhoFromFrame(frame).get(key);
    }

    public static void extendRho(VirtualFrame frame, String key, RuntimeValue value) {
        replaceRho(frame, obtainRhoFromFrame(frame).extend(key, value));
    }

    public static void replaceRho(VirtualFrame frame, Rho newRho) {
        frame.setObject(obtainRhoFrameSlot(frame), newRho);
    }

    public static String escape(String s) {
        return s.chars().mapToObj(i -> (char) i).map(Utils::escapedChar).collect(Collectors.joining());
    }

    private static String escapedChar(char c) {
        switch (c) {
            case '\b':
                return "\\b";
            case '\t':
                return "\\t";
            case '\n':
                return "\\n";
            case '\f':
                return "\\f";
            case '\r':
                return "\\r";
            case '"':
                return "\\\"";
            case '\'':
                return "\\\'";
            case '\\':
                return "\\\\";
            default:
                return (Character.isISOControl(c)) ? "\\" + Integer.toOctalString(c) : String.valueOf(c);
        }
    }
}
