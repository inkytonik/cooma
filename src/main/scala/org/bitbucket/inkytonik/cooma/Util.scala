package org.bitbucket.inkytonik.cooma

object Util {

    private var freshCount = 0

    def fresh(prefix : String) : String = {
        freshCount = freshCount + 1
        s"$prefix$freshCount"
    }

}
