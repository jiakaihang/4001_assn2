package ass02

object PropParsersTest extends PropParsers {
	
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
		"(p <+> (q <+> r)) <=> ((p <+> q) <+> r)"
	)
	
	def testParser (parser : Parser [_]) {
		for (in <- strings) {
			print ("Input: "); println (in)
			val result = parseAll (parser, in)
			println (if (result.isEmpty) "Failed" else  result.toString)
			println
		}
	}

	def testExp = testParser (prop)

	def testOBDD {
		import Operations.{vars, obdd}
		import OBDD_PP.prettyPrint
		
		def prop2OBDD = (prop : Proposition) => obdd (vars (prop).toList) (prop)
		def prop2PP = (prop : Proposition) => prettyPrint (prop2OBDD (prop), 0)
		
		val parser = prop ^^ prop2PP
		testParser (parser)
	}
	
	def main (args : Array[String])
	{
		Tester (args) (Seq (testExp _, testOBDD _))
	}
}