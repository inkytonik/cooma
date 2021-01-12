/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.cooma;

import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.runtime.CoomaContext;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

public class Utils {

    @SuppressWarnings("unchecked")
    public static RuntimeValue obtainFromRho(CoomaContext coomaContext, String key) {
        return coomaContext.getRho().get(key);
    }

    public static void extendRho(CoomaContext context, String key, RuntimeValue value) {
        replaceRho(context, context.getRho().extend(key, value));
    }

    public static void replaceRho(CoomaContext context, Rho newRho) {
        context.setRho(newRho);
    }


}
