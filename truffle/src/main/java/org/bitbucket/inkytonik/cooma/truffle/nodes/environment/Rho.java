/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

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


    public RuntimeValue get(String key) {
        return rho.get(key);
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
