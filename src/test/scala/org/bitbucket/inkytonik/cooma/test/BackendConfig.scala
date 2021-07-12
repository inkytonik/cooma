package org.bitbucket.inkytonik.cooma.test

import java.io.{ByteArrayOutputStream, PrintStream}

import org.bitbucket.inkytonik.cooma.{Frontend, ReferenceFrontend}
import org.bitbucket.inkytonik.cooma.truffle.TruffleFrontend

case class BackendConfig(name : String, frontend : Frontend, options : Seq[String])

object BackendConfig {

    lazy val truffleOutContent = new ByteArrayOutputStream

    lazy val backends = Seq(
        BackendConfig("Reference", new ReferenceFrontend, Seq()),
        BackendConfig("GraalVM", new TruffleFrontend(out = new PrintStream(truffleOutContent)), Seq("-g"))
    )

}
