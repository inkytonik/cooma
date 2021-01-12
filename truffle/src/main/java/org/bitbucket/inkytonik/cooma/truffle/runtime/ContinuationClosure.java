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

import de.uka.ilkd.pp.DataLayouter;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.bitbucket.inkytonik.cooma.truffle.nodes.environment.Rho;
import org.bitbucket.inkytonik.cooma.truffle.nodes.term.CoomaTermNode;
import java.util.Comparator;

@Getter
@RequiredArgsConstructor
public class ContinuationClosure extends RuntimeValue implements Comparable<ContinuationClosure>{
    private final Rho rho;
    private final String x;
    private final CoomaTermNode z;

    @Override
    public String toString() {
        return "<function>";
    }

    @Override
    public <Exc extends java.lang.Exception> void prettyPrint(DataLayouter<Exc> l) throws Exc {
        l.print(this.toString());
    }


    @Override
    public int compareTo(ContinuationClosure continuationClosure) {
        return Comparator.comparing(ContinuationClosure::getX).compare(this, continuationClosure);
    }

}
