package org.bitbucket.inkytonik.cooma

import org.bitbucket.inkytonik.kiama.util.{Position, Source}

class CoomaPosition(override val line : Int, override val column : Int, override val source : Source) extends Position(line : Int, column : Int, source : Source) {

    val adjustedLine : Int = line - Predef.predefLinesLength

    override lazy val format : String = {
        val name = if (source.name == "") "" else s"${source.name}:"
        s"$name$adjustedLine:$column:"
    }

}

object CoomaPosition {
    def fromPosition(pos : Position) : CoomaPosition = {
        new CoomaPosition(pos.line, pos.column, pos.source)
    }
}
