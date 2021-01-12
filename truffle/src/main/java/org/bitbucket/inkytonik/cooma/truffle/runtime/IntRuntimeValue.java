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

import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import de.uka.ilkd.pp.DataLayouter;
import java.math.BigInteger;
import java.util.Comparator;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
@ExportLibrary(InteropLibrary.class)
public class IntRuntimeValue extends RuntimeValue implements TruffleObject, Comparable<IntRuntimeValue>{
    private final BigInteger innerValue;

    @Override
    public String toString() {
        return String.valueOf(innerValue);
    }

    @Override
    public int compareTo(IntRuntimeValue intRuntimeValue) {
        return this.innerValue.compareTo(intRuntimeValue.getInnerValue());
    }

    @Override
    public <Exc extends java.lang.Exception> void prettyPrint(DataLayouter<Exc> l) throws Exc {
        l.print(this.toString());
    }

    @SuppressWarnings("static-method")
    @ExportMessage
    boolean isNumber() {
        return true;
    }

    @ExportMessage
    boolean fitsInByte() {
        return innerValue.bitLength() < 8;
    }

    @ExportMessage
    boolean fitsInShort() {
        return innerValue.bitLength() < 16;
    }

    @ExportMessage
    boolean fitsInFloat() {
        return false;
    }

    @ExportMessage
    boolean fitsInLong() {
        return innerValue.bitLength() < 64;
    }

    @ExportMessage
    boolean fitsInInt() {
        return innerValue.bitLength() < 32;
    }

    @ExportMessage
    boolean fitsInDouble() {
        return false;
    }

    @ExportMessage
    double asDouble() throws UnsupportedMessageException {
        throw UnsupportedMessageException.create();
    }

    @ExportMessage
    long asLong() throws UnsupportedMessageException {
        return innerValue.longValue();
    }

    @ExportMessage
    byte asByte() throws UnsupportedMessageException {
        return innerValue.byteValue();
    }

    @ExportMessage
    int asInt() throws UnsupportedMessageException {
        return innerValue.intValue();
    }

    @ExportMessage
    float asFloat() throws UnsupportedMessageException {
        throw UnsupportedMessageException.create();
    }

    @ExportMessage
    short asShort() throws UnsupportedMessageException {
        return innerValue.shortValue();
    }

    @ExportMessage
    public boolean isExecutable() {
        return true;
    }

    @ExportMessage final Object execute(Object[] arguments) throws UnsupportedTypeException, ArityException, UnsupportedMessageException {
        return this;
    }

}
