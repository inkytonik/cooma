package org.bitbucket.inkytonik.cooma.truffle.nodes.term;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.bitbucket.inkytonik.cooma.truffle.runtime.ContinuationClosure;

@NodeInfo(shortName = "letc", description = "Continuation declaration")
public class CoomaLetCTermNode extends CoomaTermNode {

    private final String identifier;

    private final String parameter;

    @Node.Child
    private CoomaTermNode t;

    @Node.Child
    private CoomaTermNode body;

    public CoomaLetCTermNode(String identifier, String parameter, CoomaTermNode t, CoomaTermNode body) {
        this.identifier = identifier;
        this.parameter = parameter;
        this.t = t;
        this.body = body;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        ContinuationClosure val = new ContinuationClosure(obtainRho(),this.parameter, this.t);
        extendRho(identifier, val);
        return body.executeGeneric(frame);

    }
}
