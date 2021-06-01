package org.bitbucket.inkytonik.cooma

case class CoomaException(message : String) extends RuntimeException(message) {

    override def toString : String =
        message

}

object CoomaException {

    def errCap(capability : String, message : String) : Nothing =
        throw CoomaException(s"CapabilityException: $capability: $message")

    def errInterp(function : String, message : String) : Nothing =
        throw CoomaException(s"InterpreterException: $function: $message")

    def errPrim(primitive : String, message : String) : Nothing =
        throw CoomaException(s"PrimitiveException: $primitive: $message")

    def errPrelude(sub : CoomaException) : Nothing =
        throw CoomaException(s"Error in prelude: ${sub.message}")

    def errPrelude(sub : String) : Nothing =
        throw CoomaException(s"Error in prelude: $sub")

    def getUnhandledMessage(e : Throwable) : String =
        s"""An error occurred. This is a bug in Cooma. Please create an issue at <https://github.com/inkytonik/cooma/issues> and include the message and stack trace below.
           |${e.getMessage}
           |${e.getStackTrace.map(s => s"    $s").mkString("\n")}
           |""".stripMargin

}

