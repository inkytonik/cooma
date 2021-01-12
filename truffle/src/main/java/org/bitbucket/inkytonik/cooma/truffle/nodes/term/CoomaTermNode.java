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
import org.bitbucket.inkytonik.cooma.truffle.nodes.CoomaNode;

public abstract class CoomaTermNode extends CoomaNode {

    /**
     * The execute method when no specialization is possible.
     */
    public abstract Object executeGeneric(VirtualFrame frame);

}
