package ass02

object PropRecognizerTest extends PropRecognizer {

  val strings = List (
    "p",
    "p&q",
    "p | q",
    "~p |~ q ",
    "p &~ q & r & s",
    "p => ~ q => r => ~s",
    "p => ~ q => r <= ~s",
    "p <= ~ q => r => ~s",
    "p <= q <=r <=> r => q => p",
    "(p <=> q) <=> r",
    "p <+> q <+> r <=> p <+> q <+> r",
    "(p <+> (q <+> r)) <=> ((p <+> q) <+> r)",
    
    "p_1 => p_2 => p_3 => p_4 => p_5 => p_6 => p_7 => p_8 => q_163"
  )

  def testParser(parser: Parser[_]) {
    for (in <- strings) {
      print ("Input: "); println (in)
      val result = parseAll (parser, in)
      println (if (result.isEmpty) "Failed" else "Passed")
      //println(result)
      println
    }
  }

  def testExp = testParser (prop)

  def main(args: Array[String]) {
    Tester (args) (Seq (testExp _))
  }
}