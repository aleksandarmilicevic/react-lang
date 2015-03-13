package react.verification.model.generic

import org.scalatest._
import dzufferey.smtlib._
import dzufferey.utils.{Logger, IO}
import react.utils.IDA

class GenericRobotTest extends FunSuite {
  
//test("init 1") {
//  val robot = GenericRobot(Resources.playground, Resources.path + "seg_simplest.txt")
//  robot.store = robot.store + (Variable("leftmotor.input").setType(Real) -> (1: Short))
//  robot.store = robot.store + (Variable("rightmotor.input").setType(Real) -> (1: Short))
//  robot.aboutTheEqns
//  robot.initSolution
//}

//test("init 2") {
//  val robot = GenericRobot(Resources.playground, Resources.path + "seg_eqns_simple_normal.txt")
//  robot.store = robot.store + (Variable("siminput0_input").setType(Real) -> (1: Short))
//  robot.store = robot.store + (Variable("siminput1_input").setType(Real) -> (1: Short))
//  robot.aboutTheEqns
//  robot.initSolution
//}

  test("make IDA file 1") {
    val robot = GenericRobot(Resources.playground, Resources.path + "seg_simplest.txt")
    val ida = new IDA(robot.constraints)
    val file = ida.makeFile
    Console.println("ida constraints saved in " + file)
  }

}
