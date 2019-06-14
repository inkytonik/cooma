package org.bitbucket.inkytonik.cooma.truffle.nodes.term;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.bitbucket.inkytonik.cooma.truffle.CoomaLanguage;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.runtime.ContinuationClosure;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

@NodeInfo(shortName = "AppC", description = "Continuation application")
public abstract class CoomaAppCTermNode extends CoomaCAppNode  {

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
        return obtainFromRho(frame, this.x).getValue();
    }

    @Specialization
    public Object execute(VirtualFrame frame) {
        ContinuationClosure closure  = obtainFromRho(frame, k);
        Rho p1 = closure.getRho()
                .extend(closure.getX(), obtainFromRho(frame, this.x));

        replaceRho(frame, p1);
        return closure.getZ().executeGeneric(frame);
    }

    boolean isHalt(){
        return CoomaLanguage.HALT.equals(this.k);
    }
}
