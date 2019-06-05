package org.bitbucket.inkytonik.cooma

import org.graalvm.polyglot.Context
import org.scalatest._

class TruffleTest extends FlatSpec {

    "val" should "be 10 " in {
        val context = Context.create()
        val result = context.eval("cooma", "")
        assert(result.asInt() === 10)
    }
}