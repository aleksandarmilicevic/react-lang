package react.verification.model.generic

import org.scalatest._
import dzufferey.smtlib._
import dzufferey.utils.Logger
import react.utils.DRealQuery.fixTypes

class ParserTest extends FunSuite {

  Logger.disallow("Typer")

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
    val (vars, sexpres) = GenericRobot.preprocess(content)
    assert(true)
    //for(s <- sexpres.get) println(s)
  }

//ex04 from Ankur:
//  if we connect two
//  four-sided beams along one edge, we get the following set of equations
//  between the various parameters of the components.
  test("parse 4") {
    val content = dzufferey.utils.IO.readTextFile(Resources.path + "ex04.txt")
    val sexpres = SExprParser.parse(content)
    assert(sexpres.isDefined)
    //for(s <- sexpres.get) println(s)
    import Utils._
    val unbounded = sexpres.get.map(s => parseFormula(s)).reduce(And(_, _))
    val fvs = unbounded.freeVariables
    val lo = Literal(-5.0)
    val hi = Literal(5.0)
    val formula = fvs.foldLeft(unbounded)( (acc, v) => And(Geq(v, lo), And(Leq(v, hi), acc)) )
    fixTypes(formula)
    //val dReal = DReal(QF_NRA, "test.smt2")
    //val dReal = DReal(QF_NRA_ODE, "test.smt2")
    //assert(dReal.testB(formula))
  }
  
  test("parse 5") {
    val content = dzufferey.utils.IO.readTextFile(Resources.path + "ex05.txt")
    val sexpres = SExprParser.parse(content)
    assert(sexpres.isDefined)
    //for(s <- sexpres.get) println(s)
    import Utils._
    val unbounded = sexpres.get.map(s => parseFormula(s)).reduce(And(_, _))
    val fvs = unbounded.freeVariables
    val lo = Literal(-5.0)
    val hi = Literal(5.0)
    val formula = fvs.foldLeft(unbounded)( (acc, v) => And(Geq(v, lo), And(Leq(v, hi), acc)) )
    fixTypes(formula)
  }

  test("parse seg simplest") {
    val robot = GenericRobot("id", Resources.playground, Resources.path + "seg_simplest.txt")
    assert(true)
  }

  test("parse arm 1") {
    val robot = GenericRobot("id", Resources.playground, Resources.path + "folded_arm.txt")
    //println(robot.modelDescription)
    assert(true)
  }

  test("parse arm 2") {
    val robot = GenericRobot("id", Resources.playground, Resources.path + "folded_arm_1param.txt")
    println(robot.modelDescription)
    assert(true)
  }
  
  test("parse arm 3") {
    val robot = GenericRobot("id", Resources.playground, Resources.path + "folded_arm_3params.txt")
    println(robot.modelDescription)
    assert(true)
  }

  test("test solver 1") {
    val r = GenericRobot("id", Resources.playground, Resources.path + "seg_simplest.txt")
    r.store = r.store + (Variable("leftmotor.input").setType(Real) -> (1: Short))
    r.store = r.store + (Variable("rightmotor.input").setType(Real) -> (1: Short))
    val bp = r.elapseBP(1000)
    assert(bp.alternatives == 1, "  alternatives: " + bp.alternatives)
  }
  
  test("parse fbl0") {
    val robot = GenericRobot("id", Resources.playground, Resources.path + "fbl0.txt")
    println(robot.modelDescription)
    assert(true)
  }

  test("parse fbl") {
    val robot = GenericRobot("id", Resources.playground, Resources.path + "fbl.txt")
    println(robot.modelDescription)
    assert(true)
  }

}

