package react.verification.model.generic

import org.scalatest._

class ParserTest extends FunSuite {

  test("parse 1: no let") {
    val sexpres = SExprParser.parse(Resources.model1NoLet)
    assert(sexpres.isDefined)
    //for(s <- sexpres.get) println(s)
  }

  test("parse 1: let") {
    val sexpres = SExprParser.parse(Resources.model1)
    assert(sexpres.isDefined)
    //for(s <- sexpres.get) println(s)
  }

  test("build robot 1: no let") {
    val sexpres = SExprParser.parse(Resources.model1NoLet).get
    val r = GenericRobot(Resources.playground, sexpres)
    println(r)
  }

  test("build robot 1: let") {
    val sexpres = SExprParser.parse(Resources.model1).get
    val r = GenericRobot(Resources.playground, sexpres)
    println(r)
  }

}

