package org.bitbucket.inkytonik.cooma.truffle.nodes.term;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.bitbucket.inkytonik.cooma.truffle.CoomaException;
import org.bitbucket.inkytonik.cooma.truffle.CoomaLanguage;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.runtime.ContinuationClosure;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

@NodeInfo(shortName = "AppC", description = "Continuation application")
public abstract class CoomaAppCTermNode extends CoomaCAppNode {

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

    @Specialization(guards = "isHalt()")
    public Object executeHalt(VirtualFrame frame) {
        return obtainFromRho(this.x);
    }

    @Specialization
    public Object execute(VirtualFrame frame) {

        RuntimeValue value = obtainFromRho(k);
        if (value instanceof ContinuationClosure) {
            ContinuationClosure closure = (ContinuationClosure) value;
            Rho p1 = closure.getRho()
                    .extend(closure.getX(), obtainFromRho(this.x));
            replaceRho(p1);
            return closure.getZ().executeGeneric(frame);
        } else {
            throw new CoomaException(String.format("interpret AppC: %s is %s", k, value.print()), this);
        }
    }

    boolean isHalt() {
        return CoomaLanguage.HALT.equals(this.k);
    }
}
