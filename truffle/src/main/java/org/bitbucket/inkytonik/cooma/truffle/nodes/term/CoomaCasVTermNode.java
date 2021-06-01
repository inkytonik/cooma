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

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.bitbucket.inkytonik.cooma.CoomaException;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.nodes.value.CoomaCaseTerm;
import org.bitbucket.inkytonik.cooma.truffle.runtime.ContinuationClosure;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.VarRuntimeValue;

@NodeInfo(shortName = "casV", description = "Multi-way case branching")
public class CoomaCasVTermNode extends CoomaTermNode {

    private final String x;
    private final CoomaCaseTerm[] cs;

    public CoomaCasVTermNode(String x, CoomaCaseTerm[] cs) {
        this.x = x;
        this.cs = cs;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        RuntimeValue value = obtainFromRho(x);
        if (value instanceof VarRuntimeValue) {
            VarRuntimeValue var = (VarRuntimeValue) value;
            String c1 = var.getC();
            for (CoomaCaseTerm kase : cs) {
                if (c1.equals(kase.getC())) {
                    String k = kase.getK();
                    RuntimeValue cont = obtainFromRho(k);
                    if (cont instanceof ContinuationClosure) {
                        ContinuationClosure closure = (ContinuationClosure) cont;
                        Rho p1 = closure.getRho()
                                .extend(closure.getX(), var.getV());
                        replaceRho(p1);
                        return closure.getZ().executeGeneric(frame);
                    } else {
                        return CoomaException.errInterp("CasV", String.format("%s is %s", k, cont.print()));
                    }
                }
            }
            return CoomaException.errInterp("CasV", String.format("can't find case for variant %s", c1));
        } else {
            return CoomaException.errInterp("CasV", String.format("CasV: %s is %s", x, value.print()));
        }
    }
}
