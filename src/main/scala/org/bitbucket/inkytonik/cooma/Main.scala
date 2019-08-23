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
import org.bitbucket.inkytonik.cooma.truffle.TruffleFrontend

object Main {
    def main(args : Array[String]) {
        val config = new Config(args)
        config.verify()
        if (config.graalVM())
            new TruffleFrontend().interpret(config)
        else
            new ReferenceFrontend().interpret(config)
    }
}