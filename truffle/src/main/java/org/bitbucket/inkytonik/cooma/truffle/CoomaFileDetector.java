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

import com.oracle.truffle.api.TruffleFile;
import org.bitbucket.inkytonik.cooma.CoomaConstants;

import java.nio.charset.Charset;

public class CoomaFileDetector implements TruffleFile.FileTypeDetector {

	@Override
	public String findMimeType(TruffleFile file) {
		String name = file.getName();
		if (name != null && name.endsWith(".cooma")) {
			return CoomaConstants.MIME_TYPE;
		}
		return null;
	}

	@Override
	public Charset findEncoding(TruffleFile file) {
		return null;
	}
}
