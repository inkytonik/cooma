package org.bitbucket.inkytonik.cooma.truffle.nodes.term

object ParserSupport {

    import xtc.util.Pair

    def emptyStringVector() : Vector[String] =
        Vector()

    def singleAndPairToStringVector(s : String, p : Pair[String]) : Vector[String] =
        pairToStringVector(p).prepended(s)

    def pairToStringVector(p : Pair[String]) : Vector[String] =
        if (p.isEmpty())
            Vector()
        else
            singleAndPairToStringVector(p.head(), p.tail())

}
