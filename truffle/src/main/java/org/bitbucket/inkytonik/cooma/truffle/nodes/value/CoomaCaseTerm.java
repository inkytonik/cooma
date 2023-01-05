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

import com.oracle.truffle.api.nodes.NodeInfo;
import lombok.Getter;
import org.bitbucket.inkytonik.cooma.truffle.nodes.CoomaNode;

@Getter
@NodeInfo(shortName = "caseTerm", description = "A single case in casV")
public class CoomaCaseTerm extends CoomaNode {

	private final String c;
	private final String k;

	public CoomaCaseTerm(String c, String k) {
		this.c = c;
		this.k = k;
	}

}
