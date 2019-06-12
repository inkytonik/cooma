package org.bitbucket.inkytonik.cooma.truffle.runtime;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class StringRuntimeValue extends RuntimeValue {
    private final String innerValue;

    @Override
    public String toString() {
        return innerValue;
    }
}
