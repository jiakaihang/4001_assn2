package ass02

object OBDD_PP {
  import OBDD._

  // JP: Should use Doc for this, maybe?
  /** display OBDD.Node in block layout */
  def prettyPrint(obdd: OBDD, startColumn: Int) {

    lazy val index = numberOff (obdd)
    lazy val ppDone = scala.collection.mutable.Set[OBDD] ()

    def result = pp (obdd, startColumn)

    def pp(node: OBDD, startColumn: Int) {
      lazy val nodeIndex = index (node)
      node match {
        case FalseOBDD            => println ("0")
        case TrueOBDD             => println ("1")
        case lit: PositiveLiteral => println (lit.name)
        case neg: NegativeLiteral => println ("~" + neg.name)
        // remaining cases : Conditional or Definition nodes

        // if node has already been displayed, just reference it
        case _ if ppDone (node)   => println ("[" + nodeIndex + "]")

        // if node is referenced but not yet displayed, then use index as identifier
        case _ if nodeIndex > 0 => {
          ppDone += node // record that pp is done for this node
          val ref = nodeIndex + "@"
          print (ref)
          ppCondDef (node, startColumn + ref.length)
        }
        case _ => ppCondDef (node, startColumn)
      }
    }

    def ppCondDef(obdd: OBDD, startColumn: Int) {
      obdd match {
        case cond: Conditional => {
          val name: String = cond.name
          val ifTrue = cond.ifTrue
          val ifFalse = cond.ifFalse

          if (ifTrue == FalseOBDD) ppHeader ("~" + name + " & ", ifFalse, startColumn)
          else if (ifTrue == TrueOBDD) ppHeader (name + " | ", ifFalse, startColumn)
          else if (ifFalse == FalseOBDD) ppHeader (name + "  & ", ifTrue, startColumn)
          else if (ifFalse == TrueOBDD) ppHeader ("~" + name + "  | ", ifTrue, startColumn)
          else {
            val branchColumn = startColumn + name.length
            print (name)
            print (" => ")
            pp (ifTrue, branchColumn + 4)
            leadInSpaces (branchColumn)
            print (" :  ")
            pp (ifFalse, branchColumn + 4)
          }
        }
        case defn: Definition => {
          ppHeader (defn.name + " == ", defn.obdd, startColumn)
        }
      }
    }

    def ppHeader(header: String, inlineNode: OBDD, startColumn: Int) {
      print (header)
      pp (inlineNode, startColumn + header.length)
    }

    def leadInSpaces(n: Int) { for (i <- 0 until n) print (' ') }

    result
  }

  /** Number off repeated Cond and Def nodes accessible from given node in a deterministic top-down order */
  def numberOff(node: OBDD): OBDD => Int = {

    val index = scala.collection.mutable.Map[OBDD, Int] ()

    import scala.collection.mutable.Set
    val seen = Set[OBDD] ()
    val duplicates = Set[OBDD] ()

    def maybeAdd(n: OBDD, remainder: => List[OBDD]) =
      if (!seen (n)) {
        seen += n
        n :: remainder
      } else {
        duplicates += n
        Nil
      }

    def list(obdd: OBDD): List[OBDD] = obdd match {
      case n: Conditional => maybeAdd (n, list (n.ifTrue) ++ list (n.ifFalse))
      case n: Definition  => maybeAdd (n, list (n.obdd))
      case _              => Nil
    }

    val nodes = list (node) // side effect of setting seen and duplicates

    var counter = 0
    for (n <- nodes; if duplicates (n)) {
      counter += 1
      index += n -> counter
    }
    index.getOrElse (_, 0)
  }

}