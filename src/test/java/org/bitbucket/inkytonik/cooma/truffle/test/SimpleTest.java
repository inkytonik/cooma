package org.bitbucket.inkytonik.cooma.truffle.test;

import org.graalvm.polyglot.Context;
import org.graalvm.polyglot.Value;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class SimpleTest {

    private Context context;

    @Before
    public void initEngine() {
        context = Context.create();
    }

    @Test
    public void shouldBeTen() {
        Value val = context.eval("cooma", "");
        assertEquals(10, val.asInt());
    }
}
