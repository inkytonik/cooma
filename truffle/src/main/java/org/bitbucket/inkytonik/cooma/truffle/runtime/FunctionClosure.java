package org.bitbucket.inkytonik.cooma.truffle.runtime;

import de.uka.ilkd.pp.DataLayouter;
import lombok.Getter;
import lombok.NonNull;
import lombok.RequiredArgsConstructor;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaTermNode;
import java.util.Comparator;

@Getter
@RequiredArgsConstructor
public final class FunctionClosure extends FunctionClosureHolder<FunctionClosure> {
    private final Rho rho;
    private final String k;
    private final String x;
    private final CoomaTermNode z;

    @Override
    public FunctionClosure get(String key) {
        return this;
    }

    @Override
    public String toString() {
        return "<function>";
    }

    @Override
    public <Exc extends java.lang.Exception> void prettyPrint(DataLayouter<Exc> l) throws Exc {
        l.print(this.toString());
    }

    @Override
    public int compareTo(@NonNull FunctionClosure functionClosure) {
        return Comparator.comparing(FunctionClosure::getK)
                .thenComparing(FunctionClosure::getX).compare(this, functionClosure);
    }

}
