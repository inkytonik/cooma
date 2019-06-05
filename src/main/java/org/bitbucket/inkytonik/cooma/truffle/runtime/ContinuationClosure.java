package org.bitbucket.inkytonik.cooma.truffle.runtime;

import lombok.RequiredArgsConstructor;
import lombok.Value;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaTermNode;

@Value
@RequiredArgsConstructor
public class ContinuationClosure {
    private final Rho rho;
    private final String x;
    private final CoomaTermNode z;
}
