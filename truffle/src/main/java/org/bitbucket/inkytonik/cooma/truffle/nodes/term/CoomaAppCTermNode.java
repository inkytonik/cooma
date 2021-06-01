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
import org.bitbucket.inkytonik.cooma.truffle.runtime.ContinuationClosure;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

@NodeInfo(shortName = "appC", description = "Continuation application")
public abstract class CoomaAppCTermNode extends CoomaTermNode {

    /**
     * Continuation
     */
    private final CoomaCont cont;

    /**
     * Continuation parameter identifier
     */
    private final String x;

    public CoomaAppCTermNode(CoomaCont cont, String x) {
        this.cont = cont;
        this.x = x;
    }

    @Specialization
    public Object execute(VirtualFrame frame) {
        if (this.cont instanceof CoomaHaltC) {
            return obtainFromRho(this.x);
        } else if (this.cont instanceof CoomaIdnC) {
            CoomaIdnC cont = (CoomaIdnC)this.cont;
            String k = cont.getName();
            RuntimeValue value = obtainFromRho(k);
            if (value instanceof ContinuationClosure) {
                ContinuationClosure closure = (ContinuationClosure) value;
                Rho p1 = closure.getRho()
                        .extend(closure.getX(), obtainFromRho(this.x));
                replaceRho(p1);
                return closure.getZ().executeGeneric(frame);
            } else {
                return CoomaException.errInterp("AppC", String.format("non-continuation closure: %s is %s", k, value.print()));
            }
        } else {
            return CoomaException.errInterp("AppC", String.format("unknown continuation kind: %s", this.cont));
        }
    }

}
