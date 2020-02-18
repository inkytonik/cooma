package org.bitbucket.inkytonik.cooma.truffle.nodes.value;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import lombok.Getter;
import org.bitbucket.inkytonik.cooma.truffle.runtime.ErrorRuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.VarRuntimeValue;

@Getter
@NodeInfo(shortName = "varV", description = "Variant value")
public class CoomaVarValueNode extends CoomaValueNode {

    private final String c;
    private final String x;

    public CoomaVarValueNode(String c, String x) {
        this.c = c;
        this.x = x;
    }

    @Override
    public RuntimeValue evaluate(VirtualFrame frame) {
        RuntimeValue innerValue = obtainFromRho(x);
        if (innerValue instanceof ErrorRuntimeValue) {
            return innerValue;
        }
        return new VarRuntimeValue(c, innerValue);
    }

}
