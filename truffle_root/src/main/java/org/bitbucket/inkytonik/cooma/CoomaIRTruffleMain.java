/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019-2023 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.cooma;

import org.bitbucket.inkytonik.cooma.truffle.TruffleFrontend;
import java.util.Arrays;
import static scala.collection.JavaConverters.collectionAsScalaIterableConverter;

public class CoomaIRTruffleMain {
	public static void main(String[] args) {
		Config config = new Config(collectionAsScalaIterableConverter(Arrays.asList(args)).asScala().toSeq());
		config.verify();
		new TruffleFrontend(System.in, System.out).interpret(config);
	}
}
