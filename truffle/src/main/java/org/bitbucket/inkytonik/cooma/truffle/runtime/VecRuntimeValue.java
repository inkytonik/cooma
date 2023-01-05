/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019-2023 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.cooma.truffle.runtime;

import com.oracle.truffle.api.interop.TruffleObject;
import de.uka.ilkd.pp.DataLayouter;
import lombok.EqualsAndHashCode;
import lombok.Value;
import scala.collection.immutable.Vector;

@Value
@EqualsAndHashCode(callSuper = false)
public class VecRuntimeValue extends RuntimeValue implements TruffleObject, Comparable<VecRuntimeValue> {

	private final Vector<RuntimeValue> vector;

	@Override
	public int compareTo(VecRuntimeValue vecRuntimeValue) {
		return vector.sameElements(vecRuntimeValue.vector) ? 0 : -1;
	}

	@Override
	public String toString() {
		return vector.mkString("[", ", ", "]");
	}

	@Override
	public <Exc extends java.lang.Exception> void prettyPrint(DataLayouter<Exc> l) throws Exc {
		l.print(this.toString());
	}

}
