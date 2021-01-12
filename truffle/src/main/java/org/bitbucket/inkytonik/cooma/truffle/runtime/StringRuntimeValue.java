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
import org.bitbucket.inkytonik.cooma.Util;

@Getter
@RequiredArgsConstructor
public class StringRuntimeValue extends RuntimeValue implements TruffleObject, Comparable<StringRuntimeValue> {
    private final String innerValue;

    @Override
    public String toString() {
        return String.format("\"%s\"", Util.escape(innerValue));
    }

    @Override
    public int compareTo(StringRuntimeValue stringRuntimeValue) {
        return innerValue.compareTo(stringRuntimeValue.getInnerValue());
    }

    @Override
    public <Exc extends java.lang.Exception> void prettyPrint(DataLayouter<Exc> l) throws Exc {
        l.print(this.toString());
    }

}
