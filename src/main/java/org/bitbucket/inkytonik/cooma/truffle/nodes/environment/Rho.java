package org.bitbucket.inkytonik.cooma.truffle.nodes.environment;

import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

import java.util.HashMap;
import java.util.Map;

public final class Rho {

    private final HashMap<String, RuntimeValue> rho;

    public Rho() {
        rho = new HashMap<>();
    }

    public Rho(HashMap<String, RuntimeValue> rho) {
        this.rho = rho;
    }

    public RuntimeValue get(String o) {
        return rho.get(o);
    }

    public Rho extend(String key, RuntimeValue value){
        HashMap<String, RuntimeValue> newRho = copy();
        newRho.put(key, value);
        return new Rho(newRho);
    }

    private HashMap<String, RuntimeValue> copy(){
        HashMap<String, RuntimeValue> newRho = new HashMap<>();
        for (Map.Entry<String, RuntimeValue> pair : rho.entrySet()) {
            newRho.put(pair.getKey(), pair.getValue());
        }
        return newRho;
    }

}
