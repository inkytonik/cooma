package org.bitbucket.inkytonik.cooma.truffle.nodes.term;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import org.bitbucket.inkytonik.cooma.truffle.CoomaException;
import org.bitbucket.inkytonik.cooma.truffle.CoomaLanguage;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.runtime.*;

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
    Object executeHalt(VirtualFrame frame) {

        FuntionClosureHolder value = obtainFromRho(frame, f);
        FunctionClosure closure = value.get(f);
        Rho p1 = closure.getRho()
                .extend(closure.getK(),
                        new ContinuationClosure(obtainRhoFromFrame(frame),
                                this.x, CoomaAppCTermNodeGen.create(CoomaLanguage.HALT, this.x)))
                .extend(closure.getX(), obtainFromRho(frame, this.x));
        replaceRho(frame, p1);
        return closure.getZ().executeGeneric(frame);
    }

    @Specialization
    Object execute(VirtualFrame frame) {

        RuntimeValue value = obtainFromRho(frame, f);
        if (value instanceof FuntionClosureHolder) {
            FunctionClosure closure = ((FuntionClosureHolder) value).get(f);
            Rho p1 = closure.getRho()
                    .extend(closure.getK(), obtainFromRho(frame, this.k))
                    .extend(closure.getX(), obtainFromRho(frame, this.x));
            replaceRho(frame, p1);
            return closure.getZ().executeGeneric(frame);

        } else if (value instanceof ErrorRuntimeValue) {
            return value;
        } else {
            throw new CoomaException(String.format("interpret AppF: %s is %s", f, value.print()), this);
        }
    }

    boolean isHalt() {
        return CoomaLanguage.HALT.equals(this.k);
    }
}
