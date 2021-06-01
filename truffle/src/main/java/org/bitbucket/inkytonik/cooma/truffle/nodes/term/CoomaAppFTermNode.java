/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.cooma.truffle.nodes.term;

import com.oracle.truffle.api.dsl.Specialization;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.bitbucket.inkytonik.cooma.CoomaException;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.runtime.FunctionClosure;
import org.bitbucket.inkytonik.cooma.truffle.runtime.FunctionClosureHolder;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

@NodeInfo(shortName = "appF", description = "Function application")
public abstract class CoomaAppFTermNode extends CoomaTermNode {

    /**
     * Function identifier
     */
    private final String f;

    /**
     * Continuation identifier
     */
    private final String k;

    /**
     * Parameter identifier
     */
    private final String x;

    public CoomaAppFTermNode(String identifier, String k, String x) {
        this.f = identifier;
        this.k = k;
        this.x = x;
    }

    @Specialization
    Object execute(VirtualFrame frame) {
        RuntimeValue value = obtainFromRho(f);
        if (value instanceof FunctionClosureHolder) {
            FunctionClosure closure = ((FunctionClosureHolder) value).get(f);
            Rho p1 = closure.getRho()
                    .extend(closure.getK(), obtainFromRho(this.k))
                    .extend(closure.getX(), obtainFromRho(this.x));
            replaceRho(p1);
            if (closure.getZ().getParent() == null){
                this.insert(closure.getZ());
            }
            return closure.getZ().executeGeneric(frame);

        } else {
            return CoomaException.errInterp("AppF", String.format("%s is %s", f, value.print()));
        }
    }

}
