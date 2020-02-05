package org.bitbucket.inkytonik.cooma.truffle.nodes.value;

import com.oracle.truffle.api.frame.VirtualFrame;
import org.bitbucket.inkytonik.cooma.truffle.nodes.CoomaNode;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

public abstract class CoomaValueNode extends CoomaNode {

    public abstract RuntimeValue evaluate(VirtualFrame frame);

}
