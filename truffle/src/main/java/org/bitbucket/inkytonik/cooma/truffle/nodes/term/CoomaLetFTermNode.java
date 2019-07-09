package org.bitbucket.inkytonik.cooma.truffle.nodes.term;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Lazy;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.nodes.value.CoomaDefTerm;
import org.bitbucket.inkytonik.cooma.truffle.runtime.FuncDefs;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

@NodeInfo(shortName = "letf", description = "Functions bindings")
public class CoomaLetFTermNode extends CoomaTermNode {

    @Children
    private final CoomaDefTerm[] defTerms;

    @Child
    private CoomaTermNode body;

    public CoomaLetFTermNode(CoomaDefTerm[] defTerms, CoomaTermNode body) {
        this.defTerms = defTerms;
        this.body = body;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        Rho p = obtainRho();
        FuncDefs defs = new FuncDefs();

        for (CoomaDefTerm tmp : defTerms) {
            defs.getDefs().put(tmp.getF(), tmp);
            p = p.extend(tmp.getF(), defs);
        }

        final Rho finalP = p;
        defs.setP2(Lazy.of(()-> finalP));
        replaceRho(finalP);

        return body.executeGeneric(frame);
    }
}
