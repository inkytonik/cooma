package org.bitbucket.inkytonik.cooma

import org.bitbucket.inkytonik.kiama.util.StringSource

object Predef {

    import org.bitbucket.inkytonik.kiama.util.Source

    val predefMap : (String, String) = ("map",
        """def map (inT : Type, v : Vector(inT), outT : Type, f : (e : inT)  outT   ) Vector(outT) = {
		  |    equal(Int, 0, prim VectorLength(inT, v)) match {
		  |        case True(_)  => []
		  |        case False(_) => prim PrependItemVector(outT, map(inT, Vectors.tail(inT, v) , outT, f), f(Vectors.head(inT, v)))
		  |    }
		  |}
		  |""".stripMargin
    )

    val predefFold : (String, String) = ("fold",
        """def fold (inT : Type, v : Vector(inT), f : (l : inT, r : inT)  inT, acc : inT    ) inT = {
		  |    equal(Int, 0, prim VectorLength(inT, v)) match {
		  |        case True(_)  => acc
		  |        case False(_) => fold(inT, Vectors.tail(inT, v), f, f(acc, Vectors.head(inT, v)))
		  |    }
		  |}
		  |""".stripMargin
    )

    val predefList = Vector(
        predefMap,
        predefFold
    )

    val predefREPL : String = {
        s"""val predef = {
		  |	   ${predefList.map(x => x._2).mkString(sys.props("line.separator"))}
		  |    val predef = {
		  |    	  ${predefList.map(x => x._1 + " = " + x._1).mkString("," + sys.props("line.separator"))}
		  |    }
		  |    predef
		  |}
		  |""".stripMargin

    }

    def withPredef(source : Source) : Source = {

        val predefSource =
            s"""
			  |{
			  |${predefREPL}
			  |${source.content}
			  |}
			  |""".stripMargin

        StringSource(predefSource, source.name)
    }

}
