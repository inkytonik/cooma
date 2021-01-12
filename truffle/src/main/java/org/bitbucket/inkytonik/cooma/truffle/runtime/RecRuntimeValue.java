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
import lombok.EqualsAndHashCode;
import lombok.Value;
import lombok.val;
import java.util.Arrays;
import java.util.Comparator;


@Value
@EqualsAndHashCode(callSuper=false)
public class RecRuntimeValue extends RuntimeValue implements TruffleObject, Comparable<RecRuntimeValue>{

    private final FieldValueRuntime[] fields;

    public RecRuntimeValue(FieldValueRuntime[] fields) {
        this.fields = fields;
    }

    @Override
    public int compareTo(RecRuntimeValue recRuntimeValue) {
        return Arrays.deepEquals(fields, recRuntimeValue.getFields()) ? 0 : -1;
    }

    @Override
    public String toString() {
        return this.pprint();
    }

    @Override
    public <Exc extends java.lang.Exception> void prettyPrint(DataLayouter<Exc> l) throws Exc {
        if (fields.length == 0)
            l.print("{}");
        else {
            l.beginCInd(0);
            l.beginCInd().print("{").brk(1, 0);
            for (int i = 0; i < fields.length; i++) {
                l.print(fields[i]);
                if (i != fields.length - 1)
                    l.print(",").brk(1, 0);
            }
            l.end().brk(1, 0).print("}");
            l.end();
        }
    }

    public static RecRuntimeValue empty() {
        return new RecRuntimeValue(new FieldValueRuntime[] {});
    }

}
