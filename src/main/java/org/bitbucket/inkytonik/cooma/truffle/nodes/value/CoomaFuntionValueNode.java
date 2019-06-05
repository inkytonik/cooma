package org.bitbucket.inkytonik.cooma.truffle.nodes.value;

import com.oracle.truffle.api.frame.VirtualFrame;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaTermNode;
import org.bitbucket.inkytonik.cooma.truffle.runtime.FunctionClosure;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

public class CoomaFuntionValueNode extends CoomaValueNode<RuntimeValue<FunctionClosure>> {

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

    public CoomaFuntionValueNode(String k, String x, CoomaTermNode body) {
        this.k = k;
        this.x = x;
        this.body = body;
    }

    @Override
    public RuntimeValue<FunctionClosure> evaluate(VirtualFrame frame) {
        return new RuntimeValue<FunctionClosure>(new FunctionClosure(obtainRhoFromFrame(frame), k, x, body)) {
        };
    }
}
