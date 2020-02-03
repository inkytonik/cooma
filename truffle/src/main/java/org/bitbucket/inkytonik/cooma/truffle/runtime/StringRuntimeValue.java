package org.bitbucket.inkytonik.cooma.truffle.runtime;

import com.oracle.truffle.api.interop.TruffleObject;
import de.uka.ilkd.pp.DataLayouter;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.bitbucket.inkytonik.cooma.Util;

@Getter
@RequiredArgsConstructor
public class StringRuntimeValue extends RuntimeValue<StringRuntimeValue> implements TruffleObject  {
    private final String innerValue;

    @Override
    public String toString() {
        return String.format("\"%s\"", Util.escape(innerValue));
    }

    @Override
    public int compareTo(StringRuntimeValue stringRuntimeValue) {
        return innerValue.compareTo(stringRuntimeValue.getInnerValue());
    }

    @Override
    public <Exc extends java.lang.Exception> void prettyPrint(DataLayouter<Exc> l) throws Exc {
        l.print(this.toString());
    }

}
