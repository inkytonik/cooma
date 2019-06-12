package org.bitbucket.inkytonik.cooma.truffle.runtime;

import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class IntRuntimeValue extends RuntimeValue {
    private final Integer innerValue;

    @Override
    public String toString() {
        return ""+ innerValue;
    }
}
