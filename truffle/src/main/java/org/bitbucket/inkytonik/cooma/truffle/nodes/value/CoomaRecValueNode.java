package org.bitbucket.inkytonik.cooma.truffle.nodes.value;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import lombok.Getter;
import org.bitbucket.inkytonik.cooma.truffle.runtime.ErrorRuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.FieldValueRuntime;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RecRuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

import java.util.Arrays;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

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
        Optional<FieldValueRuntime> error = fieldRL.stream().filter(f -> f.getV() instanceof ErrorRuntimeValue).findFirst();
        if (error.isPresent()){
            return error.get().getV();
        }
        return new RecRuntimeValue(fieldRL.toArray(new FieldValueRuntime[fs.length]));
    }

}
