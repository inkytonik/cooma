package org.bitbucket.inkytonik.cooma.truffle.runtime;

import com.oracle.truffle.api.interop.TruffleObject;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class ErrorRuntimeValue extends RuntimeValue<ErrorRuntimeValue> implements TruffleObject {

    private final String message;

    @Override
    public int compareTo(ErrorRuntimeValue errorRuntimeValue) {
        return errorRuntimeValue.message.compareTo(this.message);
    }

    @Override
    public String print() {
        return String.format("cooma: %s", this.message);
    }
}
