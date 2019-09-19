package org.bitbucket.inkytonik.cooma.truffle.nodes.value;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import lombok.Getter;
import org.bitbucket.inkytonik.cooma.truffle.nodes.primitives.Primitive;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

@Getter
@NodeInfo(shortName = "prmV", description = "Primitive value")
public class CoomaPrimitiveValue extends CoomaValueNode {

    private final String[] xs;
    @Node.Child
    private Primitive p;

    public CoomaPrimitiveValue(Primitive p, String[] xs) {
        this.p = p;
        this.xs = xs;
    }

    @Override
    public RuntimeValue evaluate(VirtualFrame frame) {
        return p.eval(obtainRho(), xs, getArgs());
    }
}
