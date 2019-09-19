package org.bitbucket.inkytonik.cooma.truffle.nodes.value;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import lombok.Getter;
import org.bitbucket.inkytonik.cooma.truffle.runtime.StringRuntimeValue;

@Getter
@NodeInfo(shortName = "strV", description = "String value")
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
