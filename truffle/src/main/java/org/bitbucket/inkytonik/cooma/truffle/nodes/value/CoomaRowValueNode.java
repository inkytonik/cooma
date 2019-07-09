package org.bitbucket.inkytonik.cooma.truffle.nodes.value;

import com.oracle.truffle.api.frame.VirtualFrame;
import org.bitbucket.inkytonik.cooma.truffle.runtime.FieldValueRuntime;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RowRuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

public class CoomaRowValueNode extends CoomaValueNode{

    private final FieldValue[] fs;


    public CoomaRowValueNode(FieldValue[] fs) {
        this.fs = fs;
    }

    @Override
    public RuntimeValue evaluate(VirtualFrame frame) {
        List<FieldValueRuntime> fieldRL =  Arrays.stream(fs).map((FieldValue fs) -> new FieldValueRuntime(fs.getF(), obtainFromRho(fs.getX()))).collect(Collectors.toList());
        return new RowRuntimeValue(fieldRL.toArray(new FieldValueRuntime[fs.length]));
    }
}
