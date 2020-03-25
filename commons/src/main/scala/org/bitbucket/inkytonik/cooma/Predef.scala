package org.bitbucket.inkytonik.cooma

import org.bitbucket.inkytonik.cooma.CoomaParserSyntax._
import org.bitbucket.inkytonik.kiama.rewriting.Rewriter.{everywhere, rewrite, rule}
import org.bitbucket.inkytonik.kiama.util.{Positions, StringSource}

object Predef {

    import org.bitbucket.inkytonik.kiama.util.Source

    val predefList = Vector(
        ("Ints",
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
			  |		gte = fun (x : Int, y : Int) prim IntGte(x, y),
			  |  	max = fun (x : Int, y : Int) {
			  |			prim IntGte(x, y) match {
			  |					case True(_)  => x
			  |					case False(_) => y
			  |				}
		  	  |		},
			  |  	mod = fun (x : Int, y : Int) {
			  |   		prim IntSub(x, prim IntMul(y, prim IntDiv(x,y)))
			  |     }
			  |}
			  |""".stripMargin
        ),
        ("Booleans",
            """val Booleans = {
			  |	and = fun (l : Boolean, r : Boolean) {
			  |		l match {
			  |			case False(_) => false
			  |			case True(_)  => r
			  |		}
			  |	},
			  | not = fun (b : Boolean) {
			  |		b match {
			  |			case False(_) => true
			  |			case True(_)  => false
			  |		}
			  |	},
			  |	or = fun (l : Boolean, r : Boolean) {
			  |		l match {
			  |			case False(_) => r
			  |			case True(_)  => true
			  |		}
			  |	}
			  |}
			  |""".stripMargin
        ),
        ("equal",
            """def equal(t : Type, l : t, r : t) Boolean = {
			  |		prim Equal(t, l, r)
			  |}
			  |""".stripMargin
        ),
        ("Vectors",
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
			  |     last = fun (t : Type, v : Vector(t)) {
			  |				prim SelectItemVector(t, v, prim IntSub(prim VectorLength(t, v), 1))
			  |     },
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
			  |
			  |""".stripMargin
        ),
        ("map",
            """def map (inT : Type, v : Vector(inT), f : (e : inT)  inT   ) Vector(inT) = {
			  |    equal(Int, 0, prim VectorLength(inT, v)) match {
			  |        case True(_)  => []
			  |        case False(_) => prim PrependItemVector(inT, map(inT, Vectors.tail(inT, v), f), f(Vectors.head(inT, v)))
			  |    }
			  |}
			  |""".stripMargin
        ),
        ("fold",
            """def fold (inT : Type, v : Vector(inT), f : (l : inT, r : inT)  inT, acc : inT    ) inT = {
			  |    equal(Int, 0, prim VectorLength(inT, v)) match {
			  |        case True(_)  => acc
			  |        case False(_) => fold(inT, Vectors.tail(inT, v), f, f(acc, Vectors.head(inT, v)))
			  |    }
			  |}
			  |""".stripMargin
        ),
        ("foldLeft",
            """def foldLeft (vt : Type, acct : Type, v : Vector(vt), f : (l : acct, r : vt)  acct, acc : acct ) acct = {
			  |    equal(Int, 0, prim VectorLength(vt, v)) match {
			  |        case True(_)  => acc
			  |        case False(_) => foldLeft(vt, acct, Vectors.tail(vt, v), f, f(acc, Vectors.head(vt, v)))
			  |    }
			  |}
			  |""".stripMargin
        ),
        ("foldRight",
            """def foldRight (vt : Type, acct : Type, v : Vector(vt), f : (r : vt, l : acct)  acct, acc : acct ) acct = {
			  |    equal(Int, 0, prim VectorLength(vt, v)) match {
			  |        case True(_)  => acc
			  |        case False(_) => foldRight(vt, acct, Vectors.init(vt, v), f, f(Vectors.last(vt, v), acc))
			  |    }
			  |}
			  |""".stripMargin
        ),
        ("Strings",
            """val Strings = {
			  |		length = fun (x : String) prim StrLength(x),
			  |		concat = fun (x : String, y : String) prim StrConcat(x, y),
			  |		substr = fun (x : String, y : Int) prim StrSubstr(x, y)
			  |}
			  |""".stripMargin
        ),
        ("reduce",
            """def reduce (vt : Type, v : Vector(vt), f : (l : vt, r : vt )  vt  ) vt = {
			  |
			  |  def doReduce(v : Vector(vt), f : (l : vt, r : vt )  vt, acc : vt) vt = {
			  |		equal(Int, 0, prim VectorLength(vt, v)) match {
			  |	        case True(_)  => acc
			  |	        case False(_) => doReduce(Vectors.tail(vt, v), f, f(acc,  Vectors.head(vt, v)))
			  |    	}
			  |	 }
			  |
			  |	equal(Int, 0, prim VectorLength(vt, v)) match {
			  |		case True(_)  => prim Exception("Cannot apply reduce on empty collection")
			  |		case False(_) => doReduce(Vectors.tail(vt, v), f, Vectors.head(vt, v))
			  |	}
			  |}
			  |""".stripMargin
        ),
        ("filter",
            """def filter (vt : Type, v : Vector(vt), p : (e : vt )  Boolean  ) Vector(vt) = {
			  |	val empty : Vector(vt) = []
			  |	equal(Int, 0, prim VectorLength(vt, v)) match {
			  |		case True(_)  => empty
			  |		case False(_) => p(Vectors.head(vt, v)) match {
			  |								case True(_)  => prim PrependItemVector(vt, filter(vt, Vectors.tail(vt, v), p), Vectors.head(vt, v))
			  |					        	case False(_) => filter(vt, Vectors.tail(vt, v), p)
			  |						}
			  |	}
			  |}
			  |""".stripMargin
        ),
        ("filterNot",
            """def filterNot (vt : Type, v : Vector(vt), p : (e : vt )  Boolean  ) Vector(vt) = {
			  |	val empty : Vector(vt) = []
			  |	equal(Int, 0, prim VectorLength(vt, v)) match {
			  |		case True(_)  => empty
			  |		case False(_) => p(Vectors.head(vt, v)) match {
			  |								case True(_)  => filterNot(vt, Vectors.tail(vt, v), p)
			  |					        	case False(_) => prim PrependItemVector(vt, filterNot(vt, Vectors.tail(vt, v), p), Vectors.head(vt, v))
			  |						}
			  |	}
			  |}
			  |""".stripMargin
        ),
        ("find",
            """def find (vt : Type, v : Vector(vt), p : (e : vt )  Boolean  ) Vector(vt) = {
			  |	val empty : Vector(vt) = []
			  |	equal(Int, 0, prim VectorLength(vt, v)) match {
			  |		case True(_)  => empty
			  |		case False(_) => p(Vectors.head(vt, v)) match {
			  |								case True(_)  => [Vectors.head(vt, v)]
			  |					        	case False(_) => find(vt, Vectors.tail(vt, v), p)
			  |						}
			  |	}
			  |}
			  |""".stripMargin
        ),
        ("forall",
            """def forall (vt : Type, v : Vector(vt), p : (e : vt )  Boolean  ) Boolean = {
			  |	equal(Int, 0, prim VectorLength(vt, v)) match {
			  |		case True(_)  => true
			  |		case False(_) => Booleans.and(p(Vectors.head(vt, v)), forall(vt, Vectors.tail(vt, v), p))
			  |	}
			  |}
			  |""".stripMargin
        ),
        ("foreach",
            """
			  |def foreach (vt : Type, v : Vector(vt), p : (e : vt )  Unit  ) Unit = {
			  |	equal(Int, 0, prim VectorLength(vt, v)) match {
			  |		case True(_)  => {}
			  |		case False(_) => {
			  |			val u = p(Vectors.head(vt, v))
			  |			foreach(vt, Vectors.tail(vt, v),p)
			  |		}
			  |	}
			  |}
			  |""".stripMargin
        ),
        ("indexOf",
            """def indexOf (vt : Type, v : Vector(vt), e : vt  ) Int = {
			  |	def inneIndexOf(vt : Type, v : Vector(vt), e : vt, i : Int  ) Int = {
			  |		equal(Int, 0, prim VectorLength(vt, v)) match {
			  |			case True(_)  => -1
			  |			case False(_) => equal(vt, e, Vectors.head(vt, v)) match {
			  |				case True(_)  => i
			  |				case False(_) => inneIndexOf(vt, Vectors.tail(vt, v) , e , Ints.add(i, 1))
			  |			}
			  |		}
			  |	}
			  |	inneIndexOf(vt, v , e , 0 )
			  |}
			  |""".stripMargin
        ),
        ("exists",
            """def exists (vt : Type, v : Vector(vt), p : (e : vt )  Boolean  ) Boolean = {
			  |		equal(Int, 0, prim VectorLength(vt, v)) match {
			  |			case True(_)  => false
			  |			case False(_) => p(Vectors.head(vt, v)) match {
			  |				case True(_)  => true
			  |				case False(_) => exists(vt, Vectors.tail(vt, v), p)
			  |			}
			  |		}
			  |	}
			  |""".stripMargin
        ),
        ("contains",
            """def contains (vt : Type, v : Vector(vt), e : vt) Boolean = {
			  |		equal(Int, 0, prim VectorLength(vt, v)) match {
			  |			case True(_)  => false
			  |			case False(_) => equal(vt, Vectors.head(vt, v), e) match {
			  |				case True(_)  => true
			  |				case False(_) => contains(vt, Vectors.tail(vt, v), e)
			  |			}
			  |		}
			  |	}
			  |""".stripMargin
        ),
        ("count",
            """def count (vt : Type, v : Vector(vt), p : (e : vt )  Boolean ) Int = {
			  |		Vectors.length(vt, filter(vt, v, p))
			  |	}
			  |""".stripMargin
        ),
        ("drop",
            """def drop (vt : Type, v : Vector(vt), n : Int ) Vector(vt) = {
			  |	Ints.lte(n, 0) match {
			  |		case True(_)  => v
			  |		case False(_) => drop(vt, Vectors.tail(vt, v), Ints.sub(n, 1))
			  |	}
			  |}
			  |""".stripMargin
        ),
        ("dropRight",
            """def dropRight (vt : Type, v : Vector(vt), n : Int ) Vector(vt) = {
			  |	Ints.lte(n, 0) match {
			  |		case True(_)  => v
			  |		case False(_) => dropRight(vt, Vectors.init(vt, v), Ints.sub(n, 1))
			  |	}
			  |}
			  |""".stripMargin
        ),
        ("dropWhile",
            """def dropWhile (vt : Type, v : Vector(vt), p : (e : vt )  Boolean ) Vector(vt) = {
			  |	val empty : Vector(vt) = []
			  |	equal(Int, 0, prim VectorLength(vt, v)) match {
			  |		case True(_)  => empty
			  |		case False(_) => p(Vectors.head(vt, v)) match {
			  |			case True(_)  => dropWhile(vt, Vectors.tail(vt, v), p)
			  |			case False(_) => v
			  |		}
			  |	}
			  |}
			  |""".stripMargin
        ),
        ("endsWith",
            """def endsWith (vt : Type, v : Vector(vt), s : Vector(vt) ) Boolean = {
			  |	def innerEndsWith (v : Vector(vt), s : Vector(vt), i : Int ) Boolean = {
			  |		val slice = Vectors.slice(Int, v, i, Vectors.length(Int, v))
			  |		equal(Vector(vt), s, slice) match {
			  |        	case True(_) 	=> true
			  |        	case False(_)	=> equal(Vector(vt), v, slice) match {
			  |		        case True(_)  => false
			  |		        case False(_) => innerEndsWith(v, s, Ints.sub(i, 1))
			  |		    }
			  |	    }
			  |	}
			  |	innerEndsWith(v, s, Ints.sub(Vectors.length(Int, v), 1))
			  |}
			  |""".stripMargin
        )
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
