package org.bitbucket.inkytonik.cooma.truffle.runtime;

import lombok.Getter;
import lombok.Setter;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Lazy;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.nodes.value.CoomaDefTerm;

import java.util.HashMap;

public class FuncDefs  extends FuntionClosureHolder{

    @Setter
    private Lazy<Rho> p2;

    @Getter
    private final HashMap<String, CoomaDefTerm> defs = new HashMap<>();


    @Override
    public FunctionClosure get(String key) {
        CoomaDefTerm def = defs.get(key);
        return new FunctionClosure(p2.get(), def.getK(), def.getX(), def.getBody());
    }
}
