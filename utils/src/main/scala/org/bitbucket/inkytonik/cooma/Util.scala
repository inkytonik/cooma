package org.bitbucket.inkytonik.cooma

object Util {

    private var freshCount = 0

    def resetFresh() {
        freshCount = 0
    }

    def fresh(prefix : String) : String = {
        freshCount = freshCount + 1
        s"$prefix$freshCount"
    }

    def unescape(s : String) : String =
        StringContext.treatEscapes(s)

    def escape(s : String) : String =
        s.flatMap(escapedChar)

    def escapedChar(ch : Char) : String =
        ch match {
            case '\b' => "\\b"
            case '\t' => "\\t"
            case '\n' => "\\n"
            case '\f' => "\\f"
            case '\r' => "\\r"
            case '"'  => "\\\""
            case '\'' => "\\\'"
            case '\\' => "\\\\"
            case _ => if (ch.isControl) "\\" + Integer.toOctalString(ch.toInt)
            else String.valueOf(ch)
        }

    def getConfigFilenamesTail(config : Config) : Array[String] = {
        import scala.collection.JavaConverters._
        val tail = config.filenames().tail.asJava
        tail.toArray(new Array[String](tail.size()))
    }
}
