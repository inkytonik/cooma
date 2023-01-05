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
import de.uka.ilkd.pp.*;

public abstract class RuntimeValue implements TruffleObject, PrettyPrintable {

	public Boolean isHostObject() {
		return true;
	}

	public String print() {
		return this.toString();
	}

	public abstract <Exc extends java.lang.Exception> void prettyPrint(DataLayouter<Exc> l) throws Exc;

	@SuppressWarnings("unchecked")
	public String pprint() {
		try {
			StringBackend backend = new StringBackend(80);
			DataLayouter layouter = new DataLayouter(backend, 2);
			layouter.print(this);
			layouter.close();
			return backend.getString();
		} catch (Exception e) {
			e.printStackTrace();
			return e.getMessage();
		}
	}
}
