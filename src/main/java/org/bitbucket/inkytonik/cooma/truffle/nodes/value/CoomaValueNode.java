package org.bitbucket.inkytonik.cooma.truffle.nodes.value;

import com.oracle.truffle.api.frame.VirtualFrame;
import org.bitbucket.inkytonik.cooma.truffle.nodes.CoomaNode;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

public abstract class CoomaValueNode<T extends RuntimeValue> extends CoomaNode {

    /**
     *
     */
    public abstract T evaluate(VirtualFrame frame);

}
