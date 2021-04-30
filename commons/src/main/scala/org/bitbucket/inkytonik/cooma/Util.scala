/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019-2021 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

package org.bitbucket.inkytonik.cooma

object Util {

    private var freshCount = 0

    def resetFresh() : Unit = {
        freshCount = 0
    }

    def fresh(prefix : String) : String = {
        freshCount = freshCount + 1
        s"$$$prefix$freshCount"
    }

    def unescape(s : String) : String =
        StringContext.processEscapes(s)

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
            case _    => String.valueOf(ch)
        }

    def getConfigFilenamesTail(config : Config) : Array[String] = {
        import scala.jdk.CollectionConverters._
        if (!config.filenames().isEmpty) {
            val tail = config.filenames().tail.asJava
            tail.toArray(new Array[String](tail.size()))
        } else {
            config.filenames().toArray
        }
    }
}
