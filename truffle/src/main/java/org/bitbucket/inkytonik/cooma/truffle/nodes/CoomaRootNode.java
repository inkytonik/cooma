/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.cooma.truffle.nodes;

import com.oracle.truffle.api.TruffleLanguage;
import com.oracle.truffle.api.frame.FrameSlot;
import com.oracle.truffle.api.frame.FrameSlotKind;
import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import com.oracle.truffle.api.nodes.RootNode;
import org.bitbucket.inkytonik.cooma.CoomaConstants;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaTermNode;
import org.bitbucket.inkytonik.cooma.truffle.runtime.CoomaContext;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

/**
 * The root of all CoomaIR execution trees.
 * It is a Truffle requirement that the tree root extends the class {@link RootNode}.
 *
 */

@NodeInfo(language = "cooma", description = "The root Node of every coomaIR AST")
public class CoomaRootNode extends RootNode {

    @Child private CoomaTermNode termNode;
    private CoomaContext context;

    public CoomaContext getContext() {
        return context;
    }

    public CoomaRootNode(TruffleLanguage<CoomaContext> language, CoomaContext coomaContext, CoomaTermNode termNode) {
        super(language);
        this.context = coomaContext;
        this.termNode = termNode;
    }

    public CoomaTermNode getTermNode() {
        return termNode;
    }

    @Override
    public Object execute(VirtualFrame frame) {
        return termNode.executeGeneric(frame);
    }
}
