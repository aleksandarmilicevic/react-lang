package react.verification.model.generic

import org.scalatest._
import dzufferey.smtlib._
import dzufferey.utils.{Logger, IO}

class SimplifyTest extends FunSuite {

  Logger.disallow("Typer")
  Logger.moreVerbose
  
//test("normalization 1") {
//  val robot = GenericRobot(Resources.playground, Resources.path + "seg_simplest.txt")
//  val simplifier = new Simplify(robot)
//  val robot2 = simplifier.normalize(None)
//  ()
//  //Console.println(robot.modelDescription)
//  //Console.println(robot2.modelDescription)
//}

//test("normalization 2") {
//  val robot = GenericRobot("r1", Resources.playground, Resources.path + "seg_eqns_simple.txt")
//  val simplifier = new Simplify(robot)
//  //val robot2 = simplifier.normalize(Some(25)) //TODO put a smaller timout
//  val robot2 = simplifier.normalize(Some(6))
//  println("before")
//  robot.aboutTheEqns
//  println("after")
//  robot2.aboutTheEqns
//  //IO.writeInFile( Resources.path + "seg_eqns_simple_normal.txt", robot2.modelDescription)
//  ()
//  //Console.println(robot.modelDescription)
//  //Console.println(robot2.modelDescription)
//}

  test("stats") {
    val robot = GenericRobot("r1", Resources.playground, Resources.path + "seg_eqns_simple_normal.txt")
    robot.aboutTheEqns
  }

  test("scaling") {
    val v1 = Variable("v1").setType(Real)
    val lo = Literal(-5.0)
    val hi = Literal(5.0)
    val fs = And(
      Leq(v1, hi),
      Geq(v1, lo),
      Eq(v1, Times(Literal(2l), v1))
    )
    val f1 = react.utils.Scaling.byCoeff(fs, Map(v1 -> 10))
    val f2 = react.utils.Scaling.byTarget(fs, Map(v1 -> 1))
    assert(f1 == f2)
  }

  /* usgin Z3 (weaker but faster)
  from z3 import *
  b,c,x = Reals('b c x')
  f = Exists(x, b*x+c==0);
  tac = Tactic('qe')
  tac = With(tac, qe_nonlinear=True)
  print tac.param_descrs() 
  f0 = Simplify(f);
  print Simplify(tac(f0).as_expr());
  */

}
