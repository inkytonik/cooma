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
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.nodes.NodeInfo;
import org.bitbucket.inkytonik.cooma.truffle.nodes.value.CoomaValueNode;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

@NodeInfo(shortName = "letV", description = "Value binding")
public final class CoomaLetVTermNode extends CoomaTermNode {

    private final String identifier;
    @Node.Child
    private CoomaValueNode value;
    @Node.Child
    private CoomaTermNode body;

    public CoomaLetVTermNode(String identifier, CoomaValueNode value, CoomaTermNode body) {
        this.identifier = identifier;
        this.value = value;
        this.body = body;
    }

    @Override
    public Object executeGeneric(VirtualFrame frame) {
        RuntimeValue val = value.evaluate(frame);
        extendRho(identifier, val);
        return body.executeGeneric(frame);
    }
}
