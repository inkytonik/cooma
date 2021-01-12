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
import org.bitbucket.inkytonik.cooma.Backend;
import org.bitbucket.inkytonik.cooma.Primitives.Primitive;
import org.bitbucket.inkytonik.cooma.truffle.runtime.RuntimeValue;
import scala.jdk.javaapi.CollectionConverters;

import java.util.Arrays;

@Getter
@RequiredArgsConstructor
@NodeInfo(shortName = "prmV", description = "Primitive value")
public class CoomaPrimitiveValue extends CoomaValueNode {

	private final Backend backend;
	private final Primitive p;
	private final String[] xs;


	@SuppressWarnings("unchecked")
	@Override
	public RuntimeValue evaluate(VirtualFrame frame) {
		return (RuntimeValue) p.eval(backend, obtainRho(),
				CollectionConverters.asScala(Arrays.asList(xs)).toVector(),
				CollectionConverters.asScala(Arrays.asList(getArgs()).iterator()).toVector());
	}
}
