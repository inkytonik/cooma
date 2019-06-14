package org.bitbucket.inkytonik.cooma.truffle.runtime;

import com.oracle.truffle.api.interop.TruffleObject;
import lombok.Getter;
import lombok.RequiredArgsConstructor;


@Getter
@RequiredArgsConstructor
public class StringRuntimeValue extends RuntimeValue implements TruffleObject, Comparable<StringRuntimeValue>  {
    private final String innerValue;

    @Override
    public String toString() {
        return innerValue;
    }

    @Override
    public int compareTo(StringRuntimeValue stringRuntimeValue) {
        return innerValue.compareTo(stringRuntimeValue.getInnerValue());
    }
}
