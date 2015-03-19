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
//  val robot = GenericRobot(Resources.playground, Resources.path + "seg_eqns_simple.txt")
//  val simplifier = new Simplify(robot)
//  //val robot2 = simplifier.normalize(Some(11)) //TODO put a timeout on Qepcad
//  val robot2 = simplifier.normalize(Some(6))
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

//test("qepcad queries") {
//  //val robot = GenericRobot(Resources.playground, Resources.path + "seg_eqns_simple.txt")
//  //val robot = GenericRobot(Resources.playground, Resources.path + "seg_eqns_preprocessed.txt")
//  val robot = GenericRobot(Resources.playground, Resources.path + "seg_eqns_trig.txt")
//  val simplifier = new Simplify(robot)
//  //simplifier.eliminationQuery
//  simplifier.splitQueries
//}

//test("slqf query") {
//  //val robot = GenericRobot(Resources.playground, Resources.path + "seg_eqns_simple.txt")
//  val robot = GenericRobot(Resources.playground, Resources.path + "seg_eqns_preprocessed.txt")
//  val simplifier = new Simplify(robot)
//  //simplifier.eliminationQuery
//  simplifier.slfqQuery
//}

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
