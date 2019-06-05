package org.bitbucket.inkytonik.cooma.truffle.nodes.term;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;


@NodeInfo(shortName = "halt", description = "Special case for continuation application")
public class CoomaHaltTermNode extends CoomaCAppNode {


    private final String identifier;

    public CoomaHaltTermNode(String identifier) {
        this.identifier = identifier;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        return obtainFromRho(frame, identifier).getValue();
    }
}
