package org.bitbucket.inkytonik.cooma.truffle.nodes.value;

import com.oracle.truffle.api.frame.VirtualFrame;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaTermNode;
import org.bitbucket.inkytonik.cooma.truffle.runtime.FunctionClosure;

public class CoomaFunctionValueNode extends CoomaValueNode {

    /**
     * Continuation identifier
     */
    private final String k;

    /**
     * Parameter identifier
     */
    private final String x;

    @Child
    private CoomaTermNode body;

    public CoomaFunctionValueNode(String k, String x, CoomaTermNode body) {
        this.k = k;
        this.x = x;
        this.body = body;
    }

    @Override
    public FunctionClosure evaluate(VirtualFrame frame) {
        return new FunctionClosure(obtainRhoFromFrame(frame), k, x, body);
    }
}
