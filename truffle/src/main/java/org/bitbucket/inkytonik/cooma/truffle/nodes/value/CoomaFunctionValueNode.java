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
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaTermNode;
import org.bitbucket.inkytonik.cooma.truffle.runtime.FunctionClosure;

@NodeInfo(shortName = "funV", description = "Function value")
public class CoomaFunctionValueNode extends CoomaValueNode {

	/**
	 * Continuation identifier
	 */
	private final String k;

	/**
	 * Parameter identifier
	 */
	private final String x;

	@Child
	private CoomaTermNode body;

	public CoomaFunctionValueNode(String k, String x, CoomaTermNode body) {
		this.k = k;
		this.x = x;
		this.body = body;
	}

	@Override
	public FunctionClosure evaluate(VirtualFrame frame) {
		return new FunctionClosure(obtainRho(), k, x, body);
	}
}
