package org.bitbucket.inkytonik.cooma.truffle.nodes.value;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import lombok.Getter;
import org.bitbucket.inkytonik.cooma.truffle.runtime.FieldValueRuntime;
import org.bitbucket.inkytonik.cooma.truffle.runtime.VarRuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

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
        return new VarRuntimeValue(c, obtainFromRho(x));
    }

}
