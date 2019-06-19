package org.bitbucket.inkytonik.cooma.truffle.nodes.value;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import lombok.Getter;
import org.bitbucket.inkytonik.cooma.truffle.primitives.Primitive;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

@Getter
@NodeInfo(shortName = "PrimV", description = "Primitive value node")
public class CoomaPrimitiveValue extends CoomaValueNode {

    private final Primitive p;

    private final String[] xs;

    public CoomaPrimitiveValue(Primitive p, String[] xs) {
        this.p = p;
        this.xs = xs;
    }

    @Override
    public RuntimeValue evaluate(VirtualFrame frame) {
        try {
            return p.eval(obtainRhoFromFrame(frame), xs);
        } catch (Exception e) {
            //TODO: Fix this exception
            return null;
        }
    }
}
