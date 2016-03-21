package react.verification.model.generic

import org.scalatest._
import dzufferey.smtlib._
import dzufferey.utils.{Logger, IO}
import react.utils.IDA

class GenericRobotTest extends FunSuite {

  test("init 1") {
    Logger.disallow("Typer")
    //Logger.moreVerbose
    //Logger.moreVerbose
    val robot = GenericRobot("r1", Resources.playground, Resources.path + "seg_simplest.txt")
    robot.store = robot.store + (Variable("leftmotor.input").setType(Real) -> (1: Short))
    robot.store = robot.store + (Variable("rightmotor.input").setType(Real) -> (1: Short))
    //robot.aboutTheEqns
    val (init, initDt) = robot.initSolution(1e-10)
    Console.println("init:    " + init.mkString(" "))
    Console.println("init dt: " + initDt.mkString(" "))
    ()
  }
  
  test("moveFor 1") {
    val robot = GenericRobot("r1", Resources.playground, Resources.path + "seg_simplest.txt")
    robot.store = robot.store + (Variable("leftmotor.input").setType(Real) -> (10: Short))
    robot.store = robot.store + (Variable("rightmotor.input").setType(Real) -> (10: Short))
    Console.println("(1) before: " + robot)
    robot.elapse(100)
    Console.println("(1) after:  " + robot)
  }

  test("moveFor 1 kinsol") {
    val robot = GenericRobot("r1", Resources.playground, Resources.path + "seg_simplest.txt")
    robot.store = robot.store + (Variable("leftmotor.input").setType(Real) -> (10: Short))
    robot.store = robot.store + (Variable("rightmotor.input").setType(Real) -> (10: Short))
    robot.useKINSOL = true
    robot.elapse(100)
    Console.println("(1k) after: " + robot)
  }

  test("moveFor 2") {
    val robot = GenericRobot("r1", Resources.playground, Resources.path + "seg_simplest.txt")
    robot.store = robot.store + (Variable("leftmotor.input").setType(Real) -> (0: Short))
    robot.store = robot.store + (Variable("rightmotor.input").setType(Real) -> (10: Short))
    Console.println("(2) before: " + robot)
    robot.elapse(100)
    Console.println("(2) after:  " + robot)
  }

  test("moveFor 2 kinsol") {
    val robot = GenericRobot("r1", Resources.playground, Resources.path + "seg_simplest.txt")
    robot.store = robot.store + (Variable("leftmotor.input").setType(Real) -> (0: Short))
    robot.store = robot.store + (Variable("rightmotor.input").setType(Real) -> (10: Short))
    robot.useKINSOL = true
    robot.elapse(100)
    Console.println("(2k) after: " + robot)
  }

  test("moveFor 3") {
    val robot = GenericRobot("r1", Resources.playground, Resources.path + "seg_simplest.txt")
    robot.store = robot.store + (Variable("leftmotor.input").setType(Real) -> (10: Short))
    robot.store = robot.store + (Variable("rightmotor.input").setType(Real) -> (0: Short))
    Console.println("(3) before: " + robot)
    robot.elapse(100)
    Console.println("(3) after:  " + robot)
  }
  
  test("moveFor 3 kinsol") {
    val robot = GenericRobot("r1", Resources.playground, Resources.path + "seg_simplest.txt")
    robot.store = robot.store + (Variable("leftmotor.input").setType(Real) -> (10: Short))
    robot.store = robot.store + (Variable("rightmotor.input").setType(Real) -> (0: Short))
    robot.useKINSOL = true
    robot.elapse(100)
    Console.println("(3k) after: " + robot)
  }
  
  test("moveFor 4") {
    val robot = GenericRobot("r1", Resources.playground, Resources.path + "seg_simplest.txt")
    robot.store = robot.store + (Variable("leftmotor.input").setType(Real) -> (-10: Short))
    robot.store = robot.store + (Variable("rightmotor.input").setType(Real) -> (10: Short))
    Console.println("(4) before: " + robot)
    robot.elapse(100)
    Console.println("(4) after:  " + robot)
  }

  test("moveFor 4 kinsol") {
    val robot = GenericRobot("r1", Resources.playground, Resources.path + "seg_simplest.txt")
    robot.store = robot.store + (Variable("leftmotor.input").setType(Real) -> (-10: Short))
    robot.store = robot.store + (Variable("rightmotor.input").setType(Real) -> (10: Short))
    robot.useKINSOL = true
    robot.elapse(100)
    Console.println("(4k) after: " + robot)
  }

//test("init 2") {
//  val robot = GenericRobot(Resources.playground, Resources.path + "seg_eqns_simple_normal.txt")
//  robot.store = robot.store + (Variable("siminput0_input").setType(Real) -> (10: Short))
//  robot.store = robot.store + (Variable("siminput1_input").setType(Real) -> (10: Short))
//  robot.aboutTheEqns
//  robot.initSolution
//}

//test("moveFor 1 kinsol,ida") {
//  val robot = GenericRobot("r1", Resources.playground, Resources.path + "seg_simplest.txt")
//  robot.store = robot.store + (Variable("leftmotor.input").setType(Real) -> (10: Short))
//  robot.store = robot.store + (Variable("rightmotor.input").setType(Real) -> (10: Short))
//  robot.useKINSOL = true
//  robot.useIDA = true
//  Console.println("(1i) before: " + robot)
//  robot.elapse(100)
//  Console.println("(1i) after:  " + robot)
//}


  test("make IDA file 0") {
    val robot = GenericRobot("r1", Resources.playground, Resources.path + "trivial.txt")
    val l = Variable("dx.input").setType(Real)
    val r = Variable("dy.input").setType(Real)
    robot.store = robot.store + (l -> (10: Short))
    robot.store = robot.store + (r -> (10: Short))

    val (init, initDt) = robot.initSolution(1e-10)

    val in = robot.inputs.map(_.v)
    val ida = new IDA(in, robot.constraints)

    //Console.println(ida.makeFile.toString)

    try {
      ida.prepare
      val (t, r1, r2) = ida.solve(0.1, robot.store.mapValues(_.toDouble), init, initDt)
      Console.println("t = " + t)
      Console.println("result:    " + r1.mkString(" "))
      Console.println("result dt: " + r2.mkString(" "))
    } finally {
      ida.clean
    }
  }


  test("make IDA file 1") {
    val robot = GenericRobot("r1", Resources.playground, Resources.path + "seg_simplest.txt")
    val l = Variable("leftmotor.input").setType(Real)
    val r = Variable("rightmotor.input").setType(Real)
    robot.store = robot.store + (l -> (1: Short))
    robot.store = robot.store + (r -> (1: Short))

    val tolerance = 1e-16
    val (init, initDt) = robot.initSolution(tolerance)

    //Console.println("init:   " + init.mkString)
    //Console.println("initDt: " + initDt.mkString)

    val in = robot.inputs.map(_.v)
    val ida = new IDA(in, robot.constraints)

    //Console.println(ida.makeFile.toString)

    try {
      ida.prepare
      val (t, r1, r2) = ida.solve(0.1, robot.store.mapValues(_.toDouble), init, initDt)
      Console.println("t = " + t)
      Console.println("result:    " + r1.mkString(" "))
      Console.println("result dt: " + r2.mkString(" "))
    } finally {
      ida.clean
    }
  }



}
