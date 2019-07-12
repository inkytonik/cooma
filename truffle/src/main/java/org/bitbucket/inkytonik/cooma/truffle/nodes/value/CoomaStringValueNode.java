package org.bitbucket.inkytonik.cooma.truffle.nodes.value;

import com.oracle.truffle.api.frame.VirtualFrame;
import org.bitbucket.inkytonik.cooma.truffle.runtime.StringRuntimeValue;

public class CoomaStringValueNode extends CoomaValueNode {

    private final String value;

    public CoomaStringValueNode(String value) {
        this.value = value;
    }

    @Override
    public StringRuntimeValue evaluate(VirtualFrame frame) {
        return new StringRuntimeValue(value);
    }
}
