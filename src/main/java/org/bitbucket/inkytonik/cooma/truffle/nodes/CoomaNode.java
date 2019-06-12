package org.bitbucket.inkytonik.cooma.truffle.nodes;

import com.oracle.truffle.api.frame.FrameSlot;
import com.oracle.truffle.api.frame.FrameSlotKind;
import com.oracle.truffle.api.frame.FrameUtil;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import org.bitbucket.inkytonik.cooma.truffle.CoomaLanguage;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

public class CoomaNode extends Node {

    private FrameSlot obtainRhoFrameSlot(VirtualFrame frame) {
        return frame.getFrameDescriptor().findOrAddFrameSlot(CoomaLanguage.RHO, null, FrameSlotKind.Object);
    }

    protected Rho obtainRhoFromFrame(VirtualFrame frame) {
        return (Rho) FrameUtil.getObjectSafe(frame, obtainRhoFrameSlot(frame));
    }

    @SuppressWarnings("unchecked")
    protected <T extends RuntimeValue> T  obtainFromRho(VirtualFrame frame, String key) {
        return (T) obtainRhoFromFrame(frame).get(key);
    }

    protected void extendRho(VirtualFrame frame, String key, RuntimeValue value) {
        replaceRho(frame, obtainRhoFromFrame(frame).extend(key, value));
    }

    protected void replaceRho(VirtualFrame frame, Rho newRho) {
        frame.setObject(obtainRhoFrameSlot(frame), newRho);
    }
}
