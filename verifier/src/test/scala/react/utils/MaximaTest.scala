package react.utils

import org.scalatest._
import dzufferey.utils.Logger

class MaximaTest extends FunSuite {

  //Logger.moreVerbose

  test("1 + 2") {
    if (Maxima.isPresent) {
      val out = Maxima("1 + 2;\n 1 + 2;")
      assert(out == "3\n3\n")
    }
  }

}
