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

import com.oracle.truffle.api.frame.VirtualFrame;
import org.bitbucket.inkytonik.cooma.truffle.runtime.FieldValueRuntime;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RecRuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

import com.oracle.truffle.api.nodes.NodeInfo;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import lombok.Getter;

@Getter
@NodeInfo(shortName = "recV", description = "Record value")
public class CoomaRecValueNode extends CoomaValueNode {

    private final CoomaFldV[] fields;

    public CoomaRecValueNode(CoomaFldV[] fields) {
        this.fields = fields;
    }

    @Override
    public RuntimeValue evaluate(VirtualFrame frame) {
        List<FieldValueRuntime> fieldRL = Arrays.stream(fields)
                .map((CoomaFldV field) -> new FieldValueRuntime(field.getF(), obtainFromRho(field.getX())))
                .collect(Collectors.toList());
        return new RecRuntimeValue(fieldRL.toArray(new FieldValueRuntime[fields.length]));
    }

}
