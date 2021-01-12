/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.cooma.exceptions;

public class CapabilityException extends Exception {

	public CapabilityException(String s) {
		super(s);
	}

	public CapabilityException(String s, Throwable throwable) {
		super(s, throwable);
	}

	public CapabilityException(Throwable throwable) {
		super(throwable);
	}
}
