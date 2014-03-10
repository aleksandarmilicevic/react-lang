package react

import react.utils.IO
import org.scalatest._

class ParserSuite extends FunSuite {
  
  val testDir = "src/test/resources/"

  val files = List( "empty.react",
                    "file01.react")

  for (f <- files) {
    test("should parse: " + f) {
      val result = Parser(f, IO.readTextFile(testDir + f))
      assert(result.isDefined, f + " " + result.toString)
    }
  }


}
