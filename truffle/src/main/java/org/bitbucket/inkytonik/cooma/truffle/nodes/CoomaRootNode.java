package org.bitbucket.inkytonik.cooma.truffle.nodes;

import com.oracle.truffle.api.frame.FrameSlot;
import com.oracle.truffle.api.frame.FrameSlotKind;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.RootNode;
import org.bitbucket.inkytonik.cooma.truffle.CoomaLanguage;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaTermNode;
import org.bitbucket.inkytonik.cooma.truffle.runtime.CoomaContext;

/**
 * The root of all CoomaIR execution trees.
 * It is a Truffle requirement that the tree root extends the class {@link RootNode}.
 *
 */

@NodeInfo(language = "cooma", description = "The root Node of every coomaIR AST")
public class CoomaRootNode extends RootNode {

    @Child private CoomaTermNode termNode;
    private CoomaContext context;


    public CoomaRootNode(CoomaLanguage language, CoomaTermNode termNode) {
        super(language);
        this.context = language.getContextReference().get();
        this.termNode = termNode;
    }

    public CoomaTermNode getTermNode() {
        return termNode;
    }

    @Override
    public Object execute(VirtualFrame frame) {
        FrameSlot frameSlot = frame.getFrameDescriptor().findOrAddFrameSlot( CoomaLanguage.RHO,null, FrameSlotKind.Object);
        frame.setObject(frameSlot, context.getRho());
        return termNode.executeGeneric(frame);
    }
}
