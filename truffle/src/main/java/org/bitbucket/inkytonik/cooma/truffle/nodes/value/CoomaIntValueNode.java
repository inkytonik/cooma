package org.bitbucket.inkytonik.cooma.truffle.nodes.value;

import com.oracle.truffle.api.frame.VirtualFrame;
import org.bitbucket.inkytonik.cooma.truffle.runtime.IntRuntimeValue;

public class CoomaIntValueNode extends CoomaValueNode {

    private final int value;

    public CoomaIntValueNode(int value) {
        this.value = value;
    }

    @Override
    public IntRuntimeValue evaluate(VirtualFrame frame) {
        return new IntRuntimeValue(value);
    }
}
