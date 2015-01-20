package react.verification.model.generic

import org.scalatest._
import dzufferey.smtlib._

class ParserTest extends FunSuite {

  test("parse 1") {
    val content = dzufferey.utils.IO.readTextFile(Resources.path + "ex01.txt")
    val sexpres = SExprParser.parse(content)
    assert(sexpres.isDefined)
    //for(s <- sexpres.get) println(s)
  }

  test("parse 2") {
    val content = dzufferey.utils.IO.readTextFile(Resources.path + "ex02.txt")
    val sexpres = SExprParser.parse(content)
    assert(sexpres.isDefined)
    //for(s <- sexpres.get) println(s)
  }

  test("parse 3") {
    val content = dzufferey.utils.IO.readTextFile(Resources.path + "ex03.txt")
    val sexpres = SExprParser.parse(content)
    assert(sexpres.isDefined)
    //for(s <- sexpres.get) println(s)
  }

  test("build robot 1") {
    val r = GenericRobot(Resources.playground, Resources.path + "ex01.txt")
    //println(r)
    assert(true)
  }

  test("build robot 2") {
    val r = GenericRobot(Resources.playground, Resources.path + "ex02.txt")
    //println(r)
    assert(true)
  }

  test("test solver 1") {
    val r = GenericRobot(Resources.playground, Resources.path + "ex03.txt")
    r.store = r.store + (Variable("speed").setType(Real) -> (1: Short))
    val bp = r.elapseBP(1000)
    println("alternatives: " + bp.alternatives)
    assert(true)
  }

}

