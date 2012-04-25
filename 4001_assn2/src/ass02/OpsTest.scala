package ass02

object OpsTest {

  val P = "P"
  val Q = "Q"
  val R = "R"
  val S = "S"
  val T = "T"

  val U = "U"
  val V = "V"
  val W = "W"

  val varsList = List[Proposition.Name] (P, Q, R, S, T)

  implicit def name2Var = Proposition.name2Var _

  val vals = Seq (False, True)

  val constantProps = Seq (True & False | True, True & False | ~True, True & False | ~(~(~(True & ~False) | True & ~True & True)))

  val props = Seq[Proposition] (
    P,
    ~P,
    P | Q,
    Q | P,
    ~P | ~Q | R,
    P & Q,
    Q & P,
    P & Q & ~R,
    P & Q | Q & R,
    Q & P | P & R,
    P & Q & R | Q & R & S | R & S & T,
    P | P & Q | P & Q & ~R,
    (P | Q) & (P | R),
    (~P | ~Q | R) & (~P | ~Q) & ~P,
    ~(~(~(~(~P | ~Q | R) | ~(~P | ~Q) | ~(~P))))
  )

  val condProps = Seq[Proposition] (
    IfThenElse (P,
      IfThenElse (Q,
        IfThenElse (R, True, False),
        Equivalent (R, True)
      ),
      IfThenElse (Q,
        IfThenElse (R,
          Equivalent (S, False),
          Equivalent (S, False)
        ),
        True
      )
    )
  )

  val varPropss = List (props, condProps)

  val allPropss = constantProps :: varPropss

  /** Generic test of function f showing argument a and result f(a) */
  def test[A, B](fname: String, f: A => B)(a: A) {
    print (fname)
    print (" (")
    print (a)
    print (") = ")
    println (f (a))
  }

  import Operations._

  // Test procedures for different operations

  def testEvalCon {
    println ("Testing evaluation of variable-free expressions")

    val testVal = test ("eval", eval (Map ())) _ // use empty binding

    vals foreach testVal
    println

    for (v <- vals) testVal (~v)
    println

    for (v <- vals; w <- vals) testVal (v & w)
    println

    for (v <- vals; w <- vals) testVal (v | w)
    println

    for (v <- vals; w <- vals) testVal (Equivalent (v, w))
    println

    for (u <- vals; v <- vals; w <- vals) testVal (IfThenElse (u, v, w))
    println

    constantProps foreach testVal
    println

  }

  /** Binding pairs: key -> value as a comman separated string */
  def bindingsAsString[Key, Value] (keys: Iterable[Key], binding: Key => Value) = {
      val pairs = for (k <- keys) yield k + "->" + binding (k)
      pairs mkString ("[", ", ", "]")
  }
  
  def testEvalVar {

    def testBinding(binding: Proposition.Name => Boolean) {
      print ("Testing evaluation with bindings ")

      println (bindingsAsString (varsList, binding))
      for (props <- varPropss)
        props foreach test ("eval", eval (binding))
      println
    }

    testBinding (Map ("P" -> true, "Q" -> true, "R" -> true, "S" -> true, "T" -> true))
    testBinding (Map ("P" -> true, "Q" -> true, "R" -> false, "S" -> true, "T" -> false))
    testBinding (Map ("P" -> true, "Q" -> false, "R" -> true, "S" -> false, "T" -> true))
    testBinding (Map ("P" -> false, "Q" -> false, "R" -> true, "S" -> false, "T" -> true))
  }

  def testReduce {
    println ("Testing expression reduction")

    constantProps foreach test ("reduce", reduce)

    println
  }

  def testSubstitute {
    println ("Testing substitution of truth values")

    println

    println ("Substituting P -> false")
    for (props <- varPropss)
      props foreach test ("substitute", substitute ("P", false))

    println

    println ("Substituting Q -> true")
    for (props <- varPropss)
      props foreach test ("substitute", substitute ("Q", true))

    println
  }

  def testSubstituteAndReduce {
    println ("Testing substitution of truth values")

    println

    println ("Substituting P -> false")
    for (props <- varPropss)
      props foreach test ("substituteAndReduce", substituteAndReduce ("P", false))

    println

    println ("Substituting Q -> true")
    for (props <- varPropss)
      props foreach test ("substituteAndReduce", substituteAndReduce ("Q", true))

    println
  }

  def testVars {
    println ("Testing free variables")

    // vars is the test function. It's SortedSet result is converted to a List
    // This avoids dependence of display on SortedSet implementation

    def varsToList = vars andThen (_.toList)

    for (props <- allPropss)
      props foreach test ("vars", varsToList)
    println
  }
  
  def testSolve {
    println ("Testing solve")

    def solutionsInVarOrder (goal: Proposition) = {
      val varsToList = vars (goal) toList
      val solutions = solve (goal)
      for (list <- solutions)
        yield for (sol <- list)
          yield bindingsAsString (varsToList, sol)
    }
    
    for (props <- allPropss)
      props foreach test ("solve", solutionsInVarOrder)
    println
  }

  def testOBDD {

    import OBDD.obdd2Exp

    // convert to OBDD and then back to proposition
    def fn(vs: List[Proposition.Name]): Proposition => Proposition =
      obdd (vs) _ andThen obdd2Exp

    println (
      """
Testing OBDD construction
Each expression "p" is converted to an OBDD using your definition of "obdd (varsList) (p)"
and then converted back to an expression using provided definition of "obdd2Exp"

The "varsList" is given in alphabetic order for all test cases except the last one

"""
    )

    for (props <- allPropss)
      props foreach test ("obdd", fn (varsList))

    val obddProp = (P <-> Q) & (R <-> S) & (T <-> U) & (V <-> W)

    test ("obdd", fn (List(P, Q, R, S, T, U, V, W))) (obddProp)
    test ("obdd", fn (List(P, T, Q, U, R, V, S, W))) (obddProp)
    println
  }

  def testOBDDpp {

    import OBDD_PP.prettyPrint

    def testPP(vs: List[Proposition.Name])(p: Proposition) = {
      print ("exp = ")
      println (p)
      val header = "obdd (exp) = "
      print (header)
      val node = obdd (vs) (p)
      prettyPrint (node, header.length)
      println
    }

    println ("""
Conversion to OBDD and pretty-print  for a given expression.
Your "obdd" function is used for the conversion (as in the last test).
The "prettyPrint" function is provided (do NOT change it!)

The first occurrence of a "Conditional (name, node1, node2)" is displayed in block form as

    name  => block form of node1
             ...
          :  block form of node2
             ...

When one of node1 or node2 is a FalseOBDD or TrueOBDD the (provided) prettyPrint
displays without showing the False or True node, as one of the forms
(treating inline operators as right associative):

    ~name  & block form of node2
    ~name  | block form of node2
     name  & block form of node1
     name  | block form of node1
		
The first occurrence of an "Definition (name, node)" is displayed in block form as
		
    name  == block form of node1
		
When an Conditional or Definition occurs more than once,
the first occurrence is preceded by a numeric identifier	
		
    nn@name
		
and later occurrences of the same node are simply displayed as a node reference	
		
    [nn]

""")
    for (props <- allPropss)
      props foreach testPP (varsList)

    val obddProp = (P <-> Q) & (R <-> S) & (T <-> U) & (V <-> W)

    testPP (List(P, Q, R, S, T, U, V, W)) (obddProp)
    testPP (List(P, T, Q, U, R, V, S, W)) (obddProp)
  }

  def main(args: Array[String]) {
    val tests = Seq (
      testEvalCon _,
      testEvalVar _,
      testReduce _,
      testSubstitute _,
      testSubstituteAndReduce _,
      testVars _,
      testSolve _,
      testOBDD _,
      testOBDDpp _
    )

    Tester (args) (tests)
  }
}
