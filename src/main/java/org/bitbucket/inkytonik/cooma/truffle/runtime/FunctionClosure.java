package org.bitbucket.inkytonik.cooma.truffle.runtime;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Value;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaTermNode;

@Getter
@RequiredArgsConstructor
public final class FunctionClosure extends FuntionClosureHolder {
    private final Rho rho;
    private final String k;
    private final String x;
    private final CoomaTermNode z;

    @Override
    public FunctionClosure get(String key) {
        return this;
    }
}

