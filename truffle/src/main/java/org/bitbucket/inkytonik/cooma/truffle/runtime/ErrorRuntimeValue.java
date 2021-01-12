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

import com.oracle.truffle.api.interop.TruffleObject;
import de.uka.ilkd.pp.DataLayouter;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

import java.util.Comparator;

@Getter
@RequiredArgsConstructor
public class ErrorRuntimeValue extends RuntimeValue implements TruffleObject,  Comparable<ErrorRuntimeValue> {

    private final String message;

    @Override
    public int compareTo(ErrorRuntimeValue errorRuntimeValue) {
        return Comparator.comparing(ErrorRuntimeValue::getMessage).compare(this, errorRuntimeValue);
    }

    @Override
    public String toString() {
        return String.format("cooma: %s", this.message);
    }

    @Override
    public <Exc extends java.lang.Exception> void prettyPrint(DataLayouter<Exc> l) throws Exc {
        l.print(this.toString());
    }

}
