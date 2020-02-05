package org.bitbucket.inkytonik.cooma.truffle.nodes.value;

import lombok.Value;

@Value
public class FieldValue {
    String f; String x;

    public FieldValue(String f, String x) {
        this.f = f;
        this.x = x;
    }
}
