package org.bitbucket.inkytonik.cooma.truffle.nodes.term;

import com.oracle.truffle.api.frame.VirtualFrame;
import org.bitbucket.inkytonik.cooma.truffle.nodes.CoomaNode;

public abstract class CoomaTermNode extends CoomaNode {

    /**
     * The execute method when no specialization is possible.
     */
    public abstract Object executeGeneric(VirtualFrame frame);

}
