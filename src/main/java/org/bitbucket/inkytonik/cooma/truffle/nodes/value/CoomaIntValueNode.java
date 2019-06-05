package org.bitbucket.inkytonik.cooma.truffle.nodes.value;

import com.oracle.truffle.api.frame.VirtualFrame;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

public class CoomaIntValueNode extends CoomaValueNode<RuntimeValue<Integer>> {

    private final int value;

    public CoomaIntValueNode(int value) {
        this.value = value;
    }

    @Override
    public RuntimeValue<Integer> evaluate(VirtualFrame frame) {
        return new RuntimeValue<Integer>(value) {};
    }
}
