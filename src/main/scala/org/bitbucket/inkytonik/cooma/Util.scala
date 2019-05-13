package org.bitbucket.inkytonik.cooma

object Util {

    private var freshCount = 0

    def fresh(prefix : String) : String = {
        freshCount = freshCount + 1
        s"$prefix$freshCount"
    }

    // https://stackoverflow.com/questions/9913971/scala-how-can-i-get-an-escaped-representation-of-a-string

    def escape(s : String) : String =
        s.flatMap(escapedChar)

    def escapedChar(ch: Char) : String =
        ch match {
            case '\b' => "\\b"
            case '\t' => "\\t"
            case '\n' => "\\n"
            case '\f' => "\\f"
            case '\r' => "\\r"
            case '"'  => "\\\""
            case '\'' => "\\\'"
            case '\\' => "\\\\"
            case _    => if (ch.isControl) "\\0" + Integer.toOctalString(ch.toInt)
                        else              String.valueOf(ch)
    }

    def unescape(s : String) : String =
        StringContext.treatEscapes(s)

}
