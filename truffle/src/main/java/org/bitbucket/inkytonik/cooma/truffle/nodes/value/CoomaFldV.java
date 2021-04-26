/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.cooma.truffle.nodes.value;

import lombok.Value;

@Value
public class CoomaFldV {

    String f;
    String x;

    public CoomaFldV(String f, String x) {
        this.f = f;
        this.x = x;
    }

    String getF() {
        return f;
    }

    String getX() {
        return x;
    }

}
