/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.cooma.truffle.nodes.environment;

import lombok.RequiredArgsConstructor;

import java.util.function.Supplier;

//https://dzone.com/articles/leveraging-lambda-expressions-for-lazy-evaluation
@RequiredArgsConstructor
public class Lazy<T> {

    private final Supplier<T> supplier;

    private volatile T value;

    public T get() {
        if (value == null) {
            synchronized (this) {
                if (value == null) {
                    value = supplier.get();
                }
            }
        }
        return value;
    }

    public void setValue(T value) {
        this.value = value;
    }

    public static <T> Lazy<T> of(Supplier<T> supplier) {
        return new Lazy<>(supplier);
    }

}
