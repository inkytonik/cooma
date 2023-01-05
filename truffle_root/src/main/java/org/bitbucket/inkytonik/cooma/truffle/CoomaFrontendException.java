/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019-2023 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.cooma.truffle;

import com.oracle.truffle.api.CompilerDirectives.TruffleBoundary;
import com.oracle.truffle.api.nodes.Node;
import com.oracle.truffle.api.TruffleException;

public class CoomaFrontendException extends RuntimeException implements TruffleException {

	private static final long serialVersionUID = 7667192711474L;

	private final Node location;

	@TruffleBoundary
	public CoomaFrontendException(String message) {
		super(message);
		location = null;
	}

	@Override
	public Node getLocation() {
		return location;
	}

}
