package org.bitbucket.inkytonik.cooma

import org.bitbucket.inkytonik.cooma.CoomaException.CoomaExceptionType

case class CoomaException(
    exceptionType : CoomaExceptionType,
    prefix : String,
    message : String
) extends Throwable {

    override def toString : String =
        s"$exceptionType: $prefix: $message"

}

object CoomaException {

    type CoomaExceptionType = CoomaExceptionType.Value

    object CoomaExceptionType extends Enumeration {
        val Cap = Value("CapabilityException")
        val Interp = Value("InterpreterException")
        val Prim = Value("PrimitiveException")
    }

    def errCap(capability : String, message : String) : Nothing =
        throw CoomaException(CoomaExceptionType.Cap, capability, message)

    def errInterp(function : String, message : String) : Nothing =
        throw CoomaException(CoomaExceptionType.Interp, function, message)

    def errPrim(primitive : String, message : String) : Nothing =
        throw CoomaException(CoomaExceptionType.Prim, primitive, message)

    def getUnhandledMessage(e : Throwable) : String =
        s"""An error occurred. This is a bug in Cooma. Please create an issue at <https://github.com/inkytonik/cooma/issues> and include the message and stack trace below.
           |${e.getMessage}
           |${e.getStackTrace.map(s => s"    $s").mkString("\n")}
           |""".stripMargin

}

