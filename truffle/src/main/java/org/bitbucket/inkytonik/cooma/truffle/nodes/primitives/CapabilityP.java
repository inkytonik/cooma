package org.bitbucket.inkytonik.cooma.truffle.nodes.primitives;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import lombok.RequiredArgsConstructor;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

@RequiredArgsConstructor
public abstract class CapabilityP extends Primitive {

    final String cap;

    @Override
    public Integer getNumargs() {
        return 1;
    }

    @Override
    public RuntimeValue run(Rho rho, String[] xs, String[] args) {
        return null;
    }

    @Override
    public String getShow() {
        return String.format("cap %s", cap);
    }

    public Object execute(VirtualFrame frame) {
        return null;
    }


}
