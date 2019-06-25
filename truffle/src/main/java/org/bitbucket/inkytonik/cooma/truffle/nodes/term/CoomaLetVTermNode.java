package org.bitbucket.inkytonik.cooma.truffle.nodes.term;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.bitbucket.inkytonik.cooma.truffle.nodes.value.CoomaValueNode;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

@NodeInfo(shortName = "letv", description = "Value bindings")
public final class CoomaLetVTermNode extends CoomaTermNode {

    private final String identifier;
    @Child
    private CoomaValueNode value;
    @Child
    private CoomaTermNode body;

    public CoomaLetVTermNode(String identifier, CoomaValueNode value, CoomaTermNode body) {
        this.identifier = identifier;
        this.value = value;
        this.body = body;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {



        RuntimeValue val = value.evaluate(frame);
        extendRho(frame, identifier, val);
        return body.executeGeneric(frame);
    }
}
