package react.examples.bmcTest

import react.Robot
import react.robot._
import react.message.Primitive.{Int16,String => MString}
import react.verification._
import react.verification.model._
import react.verification.model.generic._
import react.verification.environment._
import react.verification.ghost._

class SymArm(port: String) extends Robot(port) {

  var counter = 0

  every(1) {
    publish("alpha", MString("value_alpha_" + counter))
    publish("beta",  MString("value_beta_" + counter))
    counter += 1
  }

}

class SymArmSolved(port: String) extends Robot(port) {

  val alpha = "10"
  val beta = "1"
  val gripper = "19"

  var counter = 0
  val period = 3000
  val sleep = 500

  every(period) {
    counter match {
      case 0 =>
        counter += 1
        publish(alpha, Int16(-21))
        Thread.sleep(sleep)
        publish(beta,  Int16(-18))
      case 1 =>
        counter += 1
        publish(alpha, Int16(-30))
        Thread.sleep(sleep)
        publish(beta,  Int16( 60))
      case 2 =>
        counter += 1
        publish(alpha, Int16( 39))
        Thread.sleep(sleep)
        publish(beta,  Int16(-17))
      case _ =>
        counter = 0
    }
  }

//on {
//  case Key.UP =>
//    publish(alpha,  Int16( 60))
//    //publish(gripper, Int16(-80))
//  case Key.DOWN =>
//    publish(alpha,  Int16(-60))
//    //publish(gripper, Int16( 80))
//  case Key.LEFT =>
//    publish(beta,  Int16( 60))
//  case Key.RIGHT =>
//    publish(beta,  Int16(-60))
//}
}

abstract class SymArmWorld(steps: Int) extends World {

  //not used
  val xMin = -1
  val xMax = 1
  val xDiscretization = 0.015625 / 4 // 4
  val yMin = -1
  val yMax = 1
  val yDiscretization = 0.015625 / 4 // 4
  val enclosed = false
  val fpDiscretization = 0.015625

  val x = 0.41
  val y0 = -0.16
  val y1 =  0.16
  //val x = 0.20
  //val y0 = -0.1
  //val y1 =  0.1
  val targetSize = 0.01
  val safeWidth = 0.05
  val targetOffset = (safeWidth - targetSize) / 2

  val safeZone = new Box2D( x, y0, 0, safeWidth, y1 - y0)

//for (i <- 1 to steps) goal( i, safeZone)                  //stays within the safe zone
//goal( 1,     new Box2D( x+targetOffset, y0, 0, targetSize, targetSize))       //start
//goal( steps, new Box2D( x+targetOffset, y1-targetSize, 0, targetSize, targetSize))  //end

  for (i <- 1 to steps) goal( i, new Box2D( x+targetOffset, y0 + (y1-y0) / (steps-1) * (i-1), 0, targetSize, targetSize) ) 

  def safe = true

  val file: String
  val i = "/arm"

  def mkArm = {
    val m1 = SymbolicGenericRobot(i, this, "verifier/src/test/resources/" + file) //the version with parametric segment
    m1.setPosition(0,0) //TODO get the right value
    m1.setOrientation(0)//TODO get the right value
    m1
  }

}

class SymTest1(steps: Int) extends SymArmWorld(steps) {
  //val file = "folded_arm_2servos.txt"
  val file = "folded_arm_2servos_simplified.txt"
  robot(new SymArm(i), mkArm)
}

class SymTest2(steps: Int) extends SymArmWorld(steps) {
  val file = "folded_arm_1servo.txt"
  robot(new SymArm(i), mkArm)
}

class SymTest3(steps: Int) extends SymArmWorld(steps) {
  //val file = "folded_arm_2servos.txt"
  val file = "folded_arm_2servos_simplified_1.txt"
  robot(new SymArm(i), mkArm)
}

object SymRun {

  def apply(test: String, args: McOptions) {
    val world = test match {
      case "1" => (() => new SymTest1(args.timeBound))
      case "2" => (() => new SymTest2(args.timeBound))
      case "3" => (() => new SymTest3(args.timeBound))
      case _ =>   (() => sys.error("unknown"))
    }
    val runner = new react.verification.McRunner(args, world)
    runner.bmc
  }

}
