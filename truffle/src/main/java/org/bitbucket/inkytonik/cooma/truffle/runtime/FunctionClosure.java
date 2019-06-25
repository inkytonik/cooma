package org.bitbucket.inkytonik.cooma.truffle.runtime;

import com.oracle.truffle.api.interop.TruffleObject;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.Value;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaTermNode;

import java.util.Comparator;

@Getter
@RequiredArgsConstructor
public final class FunctionClosure extends FuntionClosureHolder implements TruffleObject, Comparable<FunctionClosure> {
    private final Rho rho;
    private final String k;
    private final String x;
    private final CoomaTermNode z;

    @Override
    public FunctionClosure get(String key) {
        return this;
    }

    @Override
    public int compareTo(FunctionClosure functionClosure) {
        return k.compareTo(functionClosure.k);
    }


    @Override
    public String print() {
        return "<function>";
    }
}

