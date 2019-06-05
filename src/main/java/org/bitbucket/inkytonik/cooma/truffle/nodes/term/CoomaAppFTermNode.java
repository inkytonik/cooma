package org.bitbucket.inkytonik.cooma.truffle.nodes.term;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import org.bitbucket.inkytonik.cooma.truffle.CoomaLanguage;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.runtime.ContinuationClosure;
import org.bitbucket.inkytonik.cooma.truffle.runtime.FunctionClosure;
import org.bitbucket.inkytonik.cooma.truffle.runtime.FuntionClosureHolder;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

public abstract class CoomaAppFTermNode extends CoomaTermNode {

    /**
     * Function identifier
     */
    private final String f;

    /**
     * Continuation identifier
     */
    private final String k;

    /**
     * Parameter identifier
     */
    private final String x;


    public CoomaAppFTermNode(String identifier, String k, String x) {
        this.f = identifier;
        this.k = k;
        this.x = x;
    }

    @Specialization(guards = "isHalt()")
    Object executeHalt(VirtualFrame frame){

        RuntimeValue<FuntionClosureHolder> value = obtainFromRho(frame, f);
        FunctionClosure closure = value.getValue().get(f);
        Rho p1 = closure.getRho()
                .extend(closure.getK(),
                    new RuntimeValue<ContinuationClosure>(new ContinuationClosure(obtainRhoFromFrame(frame),
                            this.x, new CoomaHaltTermNode(this.x))) {
                    })
                .extend(closure.getX(), obtainFromRho(frame, this.x));
        replaceRho(frame, p1);
        return closure.getZ().executeGeneric(frame);

    }

    @Specialization
    Object execute(VirtualFrame frame) {
        RuntimeValue<FuntionClosureHolder> value = obtainFromRho(frame, f);
        FunctionClosure closure = value.getValue().get(f);
        Rho p1 = closure.getRho()
                .extend(closure.getK(), obtainFromRho(frame, this.k))
                .extend(closure.getX(), obtainFromRho(frame, this.x));
        replaceRho(frame, p1);
        return closure.getZ().executeGeneric(frame);
    }

    boolean isHalt(){
        return CoomaLanguage.HALT.equals(this.k);
    }
}
