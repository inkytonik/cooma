package org.bitbucket.inkytonik.cooma.truffle.runtime;

import lombok.RequiredArgsConstructor;
import lombok.Value;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaTermNode;

@Value
@RequiredArgsConstructor
public final class FunctionClosure implements FuntionClosureHolder{
    private final Rho rho;
    private final String k;
    private final String x;
    private final CoomaTermNode z;

    @Override
    public FunctionClosure get(String key) {
        return this;
    }
}

