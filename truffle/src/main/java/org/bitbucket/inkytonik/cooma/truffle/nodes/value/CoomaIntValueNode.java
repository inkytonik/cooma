package org.bitbucket.inkytonik.cooma.truffle.nodes.value;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import java.math.BigInteger;
import org.bitbucket.inkytonik.cooma.truffle.runtime.IntRuntimeValue;

@NodeInfo(shortName = "intV", description = "Integer value")
public class CoomaIntValueNode extends CoomaValueNode {

    private final BigInteger value;

    public CoomaIntValueNode(BigInteger value) {
        this.value = value;
    }

    @Override
    public IntRuntimeValue evaluate(VirtualFrame frame) {
        return new IntRuntimeValue(value);
    }
}
