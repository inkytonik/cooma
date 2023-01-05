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

import de.uka.ilkd.pp.DataLayouter;
import de.uka.ilkd.pp.PrettyPrintable;
import lombok.Value;

@Value
public class FieldValueRuntime implements Comparable<FieldValueRuntime>, PrettyPrintable {
	private final String x;
	private final RuntimeValue v;

	@Override
	public String toString() {
		return String.format("%s = %s", x, v.print());
	}

	@Override
	public int compareTo(FieldValueRuntime fieldValueRuntime) {
		int xc = x.compareTo(fieldValueRuntime.getX());
		if (xc != 0)
			return xc;
		return this.v.equals(fieldValueRuntime.getV()) ? 0 : -1;
	}

	@Override
	public <Exc extends java.lang.Exception> void prettyPrint(DataLayouter<Exc> l) throws Exc {
		l.print(x).print(" = ").print(v);
	}

}
