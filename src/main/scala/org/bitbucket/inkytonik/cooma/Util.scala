/*
 * This file is part of Cooma.
 *
 * Copyright (C) 2019 Anthony M Sloane, Macquarie University.
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 */

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

}
