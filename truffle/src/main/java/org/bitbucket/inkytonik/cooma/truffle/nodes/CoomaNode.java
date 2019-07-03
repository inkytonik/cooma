package org.bitbucket.inkytonik.cooma.truffle.nodes;

import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameSlot;
import com.oracle.truffle.api.frame.FrameSlotKind;
import com.oracle.truffle.api.frame.FrameUtil;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import org.bitbucket.inkytonik.cooma.Utils;
import org.bitbucket.inkytonik.cooma.truffle.CoomaLanguage;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.runtime.CoomaContext;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;
import org.graalvm.polyglot.Value;

public class CoomaNode extends Node {

    private FrameSlot obtainRhoFrameSlot(VirtualFrame frame) {
        return Utils.obtainRhoFrameSlot(frame);
    }

    protected Rho obtainRhoFromFrame(VirtualFrame frame) {
        return Utils.obtainRhoFromFrame(frame);
    }

    @SuppressWarnings("unchecked")
    protected <T extends RuntimeValue> T  obtainFromRho(VirtualFrame frame, String key) {
        return Utils.obtainFromRho(frame, key);
    }

    protected void extendRho(VirtualFrame frame, String key, RuntimeValue value) {
        Utils.extendRho(frame, key, value);
    }

    protected void replaceRho(VirtualFrame frame, Rho newRho) {
        Utils.replaceRho(frame, newRho);
    }

    protected TruffleLanguage.ContextReference<CoomaContext> getContext(){
        return super.lookupContextReference(CoomaLanguage.class);
    }

    protected String[] getArgs(){
        return getContext().get().getEnv().getApplicationArguments();
    }
}
