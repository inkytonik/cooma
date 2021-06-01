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
import com.oracle.truffle.api.nodes.NodeInfo;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;
import org.bitbucket.inkytonik.cooma.truffle.runtime.VecRuntimeValue;
import scala.collection.Iterator;
import scala.collection.immutable.Vector;
import scala.collection.immutable.VectorBuilder;

@Getter
@RequiredArgsConstructor
@NodeInfo(shortName = "vecV", description = "Vector value")
public class CoomaVecValueNode extends CoomaValueNode {

	private final Vector<String> vector;

	@Override
	public RuntimeValue evaluate(VirtualFrame frame) {
		VectorBuilder<RuntimeValue> values = new VectorBuilder<RuntimeValue>();
		Iterator<String> iter = vector.iterator();
		while (iter.hasNext()) {
			RuntimeValue value = obtainFromRho(iter.next());
			values.addOne(value);
		}
		return new VecRuntimeValue(values.result());
	}

}
