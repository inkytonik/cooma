package org.bitbucket.inkytonik.cooma.truffle.runtime;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Value;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaTermNode;

@Getter
@RequiredArgsConstructor
public class ContinuationClosure extends RuntimeValue {
    private final Rho rho;
    private final String x;
    private final CoomaTermNode z;

    @Override
    public String print() {
        return "<function>";
    }
}