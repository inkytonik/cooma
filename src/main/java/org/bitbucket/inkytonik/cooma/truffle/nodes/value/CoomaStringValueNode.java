package org.bitbucket.inkytonik.cooma.truffle.nodes.value;

import com.oracle.truffle.api.frame.VirtualFrame;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

public class CoomaStringValueNode extends CoomaValueNode<RuntimeValue<String>> {

    private final String value;

    public CoomaStringValueNode(String value) {
        this.value = value;
    }

    @Override
    public RuntimeValue<String> evaluate(VirtualFrame frame) {
        return new RuntimeValue<String>(value) {};
    }
}
