package org.bitbucket.inkytonik.cooma.truffle.runtime;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaTermNode;

import java.util.Comparator;

@Getter
@RequiredArgsConstructor
public class ContinuationClosure extends RuntimeValue<ContinuationClosure> {
    private final Rho rho;
    private final String x;
    private final CoomaTermNode z;

    @Override
    public String print() {
        return "<function>";
    }

    @Override
    public int compareTo(ContinuationClosure continuationClosure) {
        return Comparator.comparing(ContinuationClosure::getX).compare(this, continuationClosure);
    }
}