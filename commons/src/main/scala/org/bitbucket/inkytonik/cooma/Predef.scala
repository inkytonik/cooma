package org.bitbucket.inkytonik.cooma

import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{everywhere, rewrite, rule}
import org.bitbucket.inkytonik.kiama.util.{Positions, StringSource}

object Predef {

    import org.bitbucket.inkytonik.kiama.util.Source

    val predefVectors : (String, String) = ("Vectors",
        """val Vectors = {
		  |		head = fun (t : Type, v : Vector(t)) {
		  |				prim SelectItemVector(t, v, 0)
		  |    		},
		  |		tail = fun (t : Type, v : Vector(t)) {
		  |  			val empty : Vector(t) = []
		  |				prim Equal(Int, 0, prim VectorLength(t, v)) match {
		  |    				case True(_) => empty
		  |					case False(_) => prim SliceVector(t, v, 1, prim VectorLength(t, v))
		  |    			}
		  |  		},
		  |		init = fun (t : Type, v : Vector(t)) {
		  |			val empty : Vector(t) = []
		  |			prim Equal(Int, 0, prim VectorLength(t, v)) match {
		  |				case True(_) => empty
		  |				case False(_) => prim SliceVector(t, v, 0, prim IntSub(prim VectorLength(t, v), 1))
		  |			}
		  |  	},
	  	  |     length = fun (t : Type, v : Vector(t)) {
		  |     		 prim VectorLength(t, v)
		  |     	},
		  |   	get = fun (t : Type, v : Vector(t), i : Int) {
		  |     		 prim SelectItemVector(t, v, i)
		  |     	},
		  |     put = fun (t : Type, v : Vector(t), i : Int, e : t) {
		  |     		 prim PutItemVector(t, v, i, e)
		  |     	},
		  |   	append = fun (t : Type, v : Vector(t), e : t) {
		  |     		 prim AppendItemVector(t, v, e)
		  |    		},
		  |   	prepend = fun (t : Type, v : Vector(t), e : t) {
		  |     		 prim AppendItemVector(t, v, e)
		  |    		},
		  |   	slice = fun (t : Type, v : Vector(t), i : Int, j : Int) {
		  |     		 prim SliceVector(t, v, i, j)
		  |    		},
		  |   	concat = fun (t : Type, v : Vector(t), vr : Vector(t)) {
		  |     		 prim ConcatVector(t, v, vr)
		  |    		}
		  |}
		  |""".stripMargin
    )

    val predefEqual : (String, String) = ("equal",
        """def equal(t : Type, l : t, r : t) Boolean = {
		  |		prim Equal(t, l, r)
		  |}
		  |""".stripMargin
    )

    val predefMap : (String, String) = ("map",
        """def map (inT : Type, v : Vector(inT), outT : Type, f : (e : inT)  outT   ) Vector(outT) = {
		  |    equal(Int, 0, prim VectorLength(inT, v)) match {
		  |        case True(_)  => []
		  |        case False(_) => prim PrependItemVector(outT, map(inT, Vectors.tail(inT, v) , outT, f), f(Vectors.head(inT, v)))
		  |    }
		  |}
		  |""".stripMargin
    )

    //TODO: reduce, filter, find, look for more combinators on collections

    /*
	reduce(v, i, op)
		if v is empty,
			i
		else
			reduce(v.tail, op(i, v.head), op)
	 */

    val predefFold : (String, String) = ("fold",
        """def fold (inT : Type, v : Vector(inT), f : (l : inT, r : inT)  inT, acc : inT    ) inT = {
		  |    equal(Int, 0, prim VectorLength(inT, v)) match {
		  |        case True(_)  => acc
		  |        case False(_) => fold(inT, Vectors.tail(inT, v), f, f(acc, Vectors.head(inT, v)))
		  |    }
		  |}
		  |""".stripMargin
    )

    val predefInts : (String, String) = ("Ints",
        """val Ints = {
		  |		abs = fun (x : Int) prim IntAbs(x),
		  |		add = fun (x : Int, y : Int) prim IntAdd(x, y),
		  |		div = fun (x : Int, y : Int) prim IntDiv(x, y),
		  |		mul = fun (x : Int, y : Int) prim IntMul(x, y),
		  |		pow = fun (x : Int, y : Int) prim IntPow(x, y),
		  |  	sub = fun (x : Int, y : Int) prim IntSub(x, y),
		  |		lt =  fun (x : Int, y : Int) prim IntLt(x, y),
		  |		lte = fun (x : Int, y : Int) prim IntLte(x, y),
		  |		gt =  fun (x : Int, y : Int) prim IntGt(x, y),
		  |		gte = fun (x : Int, y : Int) prim IntGte(x, y)
		  |}
		  |""".stripMargin
    )

    val predefStrings : (String, String) = ("Strings",
        """val Strings = {
		  |		length = fun (x : String) prim StrLength(x),
		  |		concat = fun (x : String, y : String) prim StrConcat(x, y),
		  |		substr = fun (x : String, y : Int) prim StrSubstr(x, y)
		  |}
		  |""".stripMargin
    )

    val predefList = Vector(
        predefEqual,
        predefVectors,
        predefMap,
        predefFold,
        predefInts,
        predefStrings
    )

    val predefREPL : String = {
        s"""val predef = {
		  |	   ${predefList.map(x => x._2).mkString(sys.props("line.separator"))}
		  |    {
		  |    	  ${predefList.map(x => x._1 + " = " + x._1).mkString("," + sys.props("line.separator"))}
		  |    }
		  |}
		  |""".stripMargin
    }

    def withPredef(source : Source) : Source = {

        val predefSource =
            s"""${predefREPL}
			  |${source.content}""".stripMargin

        StringSource(predefSource, source.name)
    }

    def movePostion(n : Any, positions : Positions) : Unit = {
        if (positions.getStart(n).isDefined && positions.getFinish(n).isDefined) {
            val curposStart = positions.getStart(n).get
            val curposFinish = positions.getFinish(n).get
            positions.resetAllAt(Vector(n))
            positions.setStart(n, CoomaPosition.fromPosition(curposStart))
            positions.setFinish(n, CoomaPosition.fromPosition(curposFinish))
        }
    }

    def rewritePositions(program : Program, positions : Positions) : Unit = {
        rewrite(everywhere(rule[ASTNode] { case n => movePostion(n, positions); n }))(program)
    }

    def userCode(program : Program) : ASTNode = {

        def findReturn(blk : BlockExp) : Program = {
            blk match {
                case BlkDef(_, blockExp) => findReturn(blockExp)
                case BlkLet(_, blockExp) => findReturn(blockExp)
                case r @ Return(_)       => Program(r)
            }
        }
        findReturn(program.blockExp)

    }

    val predefLinesLength : Int = predefREPL.split(sys.props("line.separator")).length + 1

}
