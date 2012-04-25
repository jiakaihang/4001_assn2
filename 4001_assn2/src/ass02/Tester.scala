package ass02

case class Tester(args: Array[String]) {

  def apply(tests: Seq[() => _]) {
    def cmdLineError {
      print("Optional command line args (0) must be between 1 and ")
      println (tests.length)
      print("Omitting args (0) runs all tests")
    }

    def testAll {
      for (i <- 1 to tests.length) testOne (i)
    }

    def testOne(i: Int) {
      println ("Test " + i)
      println ("=======")
      try {
        tests (i - 1) ()
      } catch {
        case e: Throwable => print ("Exception thrown : " + e)
      }
    }

    if (args.isEmpty) {
      testAll
    } else {
      val i = args (0).toInt
      if (i <= 0 || i > tests.length) cmdLineError else testOne (i)
    }

    println ("--------------- end of output ---------------")
  }
}