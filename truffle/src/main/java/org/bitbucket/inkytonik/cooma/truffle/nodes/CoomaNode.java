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
import com.oracle.truffle.api.nodes.Node;
import org.bitbucket.inkytonik.cooma.Utils;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.runtime.CoomaContext;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

public class CoomaNode extends Node {

    protected Rho obtainRho(){
        return getContext().getRho();
    }

    protected RuntimeValue obtainFromRho(String key) {
        return Utils.obtainFromRho(getContext(), key);
    }

    protected void extendRho(String key, RuntimeValue value) {
        Utils.extendRho(getContext(), key, value);
    }

    protected void replaceRho(Rho newRho) {
        Utils.replaceRho(getContext(), newRho);
    }

    protected CoomaContext getContext(){
        return ((CoomaRootNode) getRootNode()).getContext();
    }

    protected String[] getArgs(){
        return getContext().getApplicationArguments();
    }
}
