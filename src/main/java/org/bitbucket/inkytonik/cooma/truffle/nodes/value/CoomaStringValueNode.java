package org.bitbucket.inkytonik.cooma.truffle.nodes.value;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.thoughtworks.xstream.annotations.XStreamConverter;
import com.thoughtworks.xstream.converters.basic.CharConverter;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.StringRuntimeValue;

public class CoomaStringValueNode extends CoomaValueNode {

    private final String value;

    public CoomaStringValueNode(String value) {
        this.value = value;
    }

    @Override
    public StringRuntimeValue evaluate(VirtualFrame frame) {
        return new StringRuntimeValue(value);
    }
}
