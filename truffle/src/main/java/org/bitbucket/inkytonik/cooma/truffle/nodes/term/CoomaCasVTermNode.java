package org.bitbucket.inkytonik.cooma.truffle.nodes.term;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.bitbucket.inkytonik.cooma.truffle.exceptions.CoomaException;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.nodes.value.CoomaCaseTerm;
import org.bitbucket.inkytonik.cooma.truffle.runtime.ContinuationClosure;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.VarRuntimeValue;

@NodeInfo(shortName = "casV", description = "Multi-way case branching")
public class CoomaCasVTermNode extends CoomaTermNode {

    private final String x;
    private final CoomaCaseTerm[] cs;

    public CoomaCasVTermNode(String x, CoomaCaseTerm[] cs) {
        this.x = x;
        this.cs = cs;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        RuntimeValue value = obtainFromRho(x);
        if (value instanceof VarRuntimeValue) {
            VarRuntimeValue var = (VarRuntimeValue) value;
            String c1 = var.getC();
            for (CoomaCaseTerm kase : cs) {
                if (c1.equals(kase.getC())) {
                    String k = kase.getK();
                    RuntimeValue cont = obtainFromRho(k);
                    if (cont instanceof ContinuationClosure) {
                        ContinuationClosure closure = (ContinuationClosure) cont;
                        Rho p1 = closure.getRho()
                                .extend(closure.getX(), var.getV());
                        replaceRho(p1);
                        return closure.getZ().executeGeneric(frame);
                    } else {
                        throw new CoomaException(String.format("interpret CasV: %s is %s", k, cont.print()), this);
                    }
                }
            }
            throw new CoomaException(String.format("interpret CasV: can't find case for variant %s", c1), this);
        } else {
            throw new CoomaException(String.format("interpret CasV: %s is %s", x, value.print()), this);
        }
    }
}
