/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019-2023 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.cooma.truffle.nodes.value;

import com.oracle.truffle.api.frame.VirtualFrame;
import com.oracle.truffle.api.nodes.NodeInfo;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;
import lombok.Getter;
import org.bitbucket.inkytonik.cooma.truffle.runtime.FieldValueRuntime;
import org.bitbucket.inkytonik.cooma.truffle.runtime.VarRuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;

@Getter
@NodeInfo(shortName = "varV", description = "Variant value")
public class CoomaVarValueNode extends CoomaValueNode {

	private final CoomaFldV fldV;

	public CoomaVarValueNode(CoomaFldV fldV) {
		this.fldV = fldV;
	}

	@Override
	public RuntimeValue evaluate(VirtualFrame frame) {
		return new VarRuntimeValue(fldV.getF(), obtainFromRho(fldV.getX()));
	}

}
