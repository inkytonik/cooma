package org.bitbucket.inkytonik.cooma.truffle.nodes.value;

import com.oracle.truffle.api.frame.VirtualFrame;
import org.bitbucket.inkytonik.cooma.truffle.runtime.FieldValueRuntime;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RecRuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

import com.oracle.truffle.api.nodes.NodeInfo;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import lombok.Getter;

@Getter
@NodeInfo(shortName = "recV", description = "Record value")
public class CoomaRecValueNode extends CoomaValueNode {

    private final FieldValue[] fs;

    public CoomaRecValueNode(FieldValue[] fs) {
        this.fs = fs;
    }

    @Override
    public RuntimeValue evaluate(VirtualFrame frame) {
        List<FieldValueRuntime> fieldRL = Arrays.stream(fs)
                .map((FieldValue fs) -> new FieldValueRuntime(fs.getF(), obtainFromRho(fs.getX())))
                .collect(Collectors.toList());
        return new RecRuntimeValue(fieldRL.toArray(new FieldValueRuntime[fs.length]));
    }

}
