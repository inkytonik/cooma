package org.bitbucket.inkytonik.cooma.truffle.nodes.term;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.bitbucket.inkytonik.cooma.truffle.runtime.ContinuationClosure;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

@NodeInfo(shortName = "letc", description = "Continuation declaration")
public class CoomaLetCTermNode extends CoomaTermNode {

    private final String identifier;

    private final String parameter;

    @Child
    private CoomaTermNode t;

    @Child
    private CoomaTermNode body;

    public CoomaLetCTermNode(String identifier, String parameter, CoomaTermNode t, CoomaTermNode body) {
        this.identifier = identifier;
        this.parameter = parameter;
        this.t = t;
        this.body = body;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        ContinuationClosure val = new ContinuationClosure(obtainRhoFromFrame(frame),this.parameter, this.t);
        extendRho(frame, identifier, val);
        return body.executeGeneric(frame);

    }
}
