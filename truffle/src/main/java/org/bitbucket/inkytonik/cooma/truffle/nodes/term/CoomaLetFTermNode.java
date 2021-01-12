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
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Lazy;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.nodes.value.CoomaDefTerm;
import org.bitbucket.inkytonik.cooma.truffle.runtime.FuncDefs;

@NodeInfo(shortName = "letF", description = "Function bindings")
public class CoomaLetFTermNode extends CoomaTermNode {

    @Children
    private final CoomaDefTerm[] defTerms;

    @Child
    private CoomaTermNode body;

    public CoomaLetFTermNode(CoomaDefTerm[] defTerms, CoomaTermNode body) {
        this.defTerms = defTerms;
        this.body = body;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        Rho p = obtainRho();
        FuncDefs defs = new FuncDefs();

        for (CoomaDefTerm tmp : defTerms) {
            defs.getDefs().put(tmp.getF(), tmp);
            p = p.extend(tmp.getF(), defs);
        }

        final Rho finalP = p;
        defs.setP2(Lazy.of(()-> finalP));
        replaceRho(finalP);

        return body.executeGeneric(frame);
    }
}
