package org.bitbucket.inkytonik.cooma.truffle.nodes.term;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.runtime.ContinuationClosure;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

@NodeInfo(shortName = "AppC", description = "Continuation application")
public class CoomaAppCTermNode extends CoomaCAppNode  {

    /**
     * Continuation identifier
     */
    private final String k;


    /**
     * Continuation parameter identifier
     */
    private final String x;

    public CoomaAppCTermNode(String k, String x) {
        this.k = k;
        this.x = x;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        RuntimeValue<ContinuationClosure> value = obtainFromRho(frame, k);
        ContinuationClosure closure = value.getValue();
        Rho p1 = closure.getRho()
                .extend(closure.getX(), obtainFromRho(frame, this.x));

        replaceRho(frame, p1);
        return closure.getZ().executeGeneric(frame);
    }
}
