package org.bitbucket.inkytonik.cooma.truffle.runtime;

import com.oracle.truffle.api.interop.TruffleObject;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class IntRuntimeValue extends RuntimeValue implements TruffleObject, Comparable<IntRuntimeValue>   {
    private final Integer innerValue;

    @Override
    public String toString() {
        return ""+ innerValue;
    }

    @Override
    public int compareTo(IntRuntimeValue intRuntimeValue) {
        return this.innerValue.compareTo(intRuntimeValue.getInnerValue());
    }
}
