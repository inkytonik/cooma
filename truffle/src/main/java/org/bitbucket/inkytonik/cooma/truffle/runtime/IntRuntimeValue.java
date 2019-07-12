package org.bitbucket.inkytonik.cooma.truffle.runtime;

import com.oracle.truffle.api.interop.*;
import com.oracle.truffle.api.library.ExportLibrary;
import com.oracle.truffle.api.library.ExportMessage;
import lombok.Getter;
import lombok.RequiredArgsConstructor;


@Getter
@RequiredArgsConstructor
@ExportLibrary(InteropLibrary.class)
public class IntRuntimeValue extends RuntimeValue<IntRuntimeValue> implements TruffleObject{
    private final Integer innerValue;

    @Override
    public String toString() {
        return String.valueOf(innerValue);
    }

    @Override
    public int compareTo(IntRuntimeValue intRuntimeValue) {
        return this.innerValue.compareTo(intRuntimeValue.getInnerValue());
    }

    @Override
    public String print() {
        return this.toString();
    }

    @SuppressWarnings("static-method")
    @ExportMessage
    boolean isNumber() {
        return fitsInLong();
    }

    @ExportMessage
    boolean fitsInByte() {
        return true;
    }

    @ExportMessage
    boolean fitsInShort() {
        return true;
    }

    @ExportMessage
    boolean fitsInFloat() {
        return true;
    }

    @ExportMessage
    boolean fitsInLong() {
        return true;
    }

    @ExportMessage
    boolean fitsInInt() {
        return true;
    }

    @ExportMessage
    boolean fitsInDouble() {
        return true;
    }

    @ExportMessage
    double asDouble() throws UnsupportedMessageException {
        return innerValue.doubleValue();
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
        return innerValue;
    }

    @ExportMessage
    float asFloat() throws UnsupportedMessageException {
        return innerValue.floatValue();
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


