/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.cooma.truffle.runtime;

import org.bitbucket.inkytonik.cooma.CoomaParserPrettyPrinterImpl;

public class JavaCoomaParserPrettyPrinter extends CoomaParserPrettyPrinterImpl {

	private static JavaCoomaParserPrettyPrinter instance;

	public static JavaCoomaParserPrettyPrinter getInstance(){
		if (instance == null) {
			instance = new JavaCoomaParserPrettyPrinter();
		}
		return instance;
	}


}
