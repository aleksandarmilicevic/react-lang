package react.verification.model.generic

import org.scalatest._
import dzufferey.smtlib._
import dzufferey.utils.{Logger, IO}

class SimplifyTest extends FunSuite {

  Logger.disallow("Typer")
//Logger.moreVerbose
  
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

//test("normalization 3") {
//  val robot = GenericRobot("r1", Resources.playground, Resources.path + "folded_arm_2servos.txt")
//  val simplifier = new Simplify(robot)
//  //val robot2 = simplifier.normalize(Some(25)) //TODO put a smaller timout
//  val robot2 = simplifier.normalize(Some(12))
//  println("before")
//  robot.aboutTheEqns
//  Console.println(robot.modelDescription)
//  println("after")
//  robot2.aboutTheEqns
//  Console.println(robot2.modelDescription)
//  //IO.writeInFile( Resources.path + "folded_arm_2servos_simplified.txt", robot2.modelDescription)
//  ()
//}

//test("normalization 4") {
//  val robot = GenericRobot("r1", Resources.playground, Resources.path + "armeqns.txt")
//  val simplifier = new Simplify(robot)
//  simplifier.qepcadSafeProjection = false
//  simplifier.qepcadTimeout = 30000
//  simplifier.qepcadBound = 12
//  val robot2 = simplifier.normalize()
//  println("before")
//  robot.aboutTheEqns
//  Console.println(robot.modelDescription)
//  println("after")
//  robot2.aboutTheEqns
//  Console.println(robot2.modelDescription)
//  IO.writeInFile( Resources.path + "armeqns_simplified.txt", robot2.modelDescription)
//  ()
//}

//test("normalization 5") {
//  val robot = GenericRobot("r1", Resources.playground, Resources.path + "seg_maxima.txt")
//  val simplifier = new Simplify(robot)
//  simplifier.qepcadSafeProjection = false
//  simplifier.qepcadTimeout = 30000
//  simplifier.qepcadBound = 12
//  val robot2 = simplifier.normalize()
//  println("before")
//  robot.aboutTheEqns
//  Console.println(robot.modelDescription)
//  println("after")
//  robot2.aboutTheEqns
//  Console.println(robot2.modelDescription)
//  IO.writeInFile( Resources.path + "seg_maxima_simplified.txt", robot2.modelDescription)
//  ()
//}

//test("stats") {
//  val robot = GenericRobot("r1", Resources.playground, Resources.path + "seg_eqns_simple_normal.txt")
//  robot.aboutTheEqns
//}

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
  
//test("normalization 6") {
//  val robot = GenericRobot("r1", Resources.playground, Resources.path + "fbl0.txt")
//  val simplifier = new Simplify(robot)
//  simplifier.qepcadSafeProjection = false
//  simplifier.qepcadTimeout = 30000
//  simplifier.qepcadBound = 12
//  val robot2 = simplifier.normalize()
//  println("before")
//  robot.aboutTheEqns
//  Console.println(robot.modelDescription)
//  println("after")
//  robot2.aboutTheEqns
//  Console.println(robot2.modelDescription)
//  IO.writeInFile( Resources.path + "fbl0_simplified.txt", robot2.modelDescription)
//  ()
//}

  test("normalization 7") {
    val robot = GenericRobot("r1", Resources.playground, Resources.path + "fbl.txt")
    val simplifier = new Simplify(robot)
    simplifier.qepcadSafeProjection = false
    simplifier.qepcadTimeout = 30000
    simplifier.qepcadBound = 12
    val robot2 = simplifier.normalize()
    println("before")
    robot.aboutTheEqns
    Console.println(robot.modelDescription)
    println("after")
    robot2.aboutTheEqns
    Console.println(robot2.modelDescription)
    IO.writeInFile( Resources.path + "fbl_simplified.txt", robot2.modelDescription)
    ()
  }

}
