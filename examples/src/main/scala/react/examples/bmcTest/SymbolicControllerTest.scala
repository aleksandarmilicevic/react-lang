package react.examples.bmcTest

import react.Robot
import react.message._
import react.verification._
import react.verification.model._
import react.verification.model.generic._
import react.verification.environment._
import react.verification.ghost._

class SymArm(port: String) extends Robot(port) {

  var counter = 0

  every(1) {
    publish("alpha", Primitive.String("alpha_value_" + counter))
    publish("beta",  Primitive.String("beta_value_" + counter))
    counter += 1
  }

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
      

  val safeZone = new Box2D( 0.20, -0.1, 0, 0.05, 0.2)

  //stays within the safe zone
  for (i <- 1 to steps) goal( i, safeZone)
  //start
  goal( 1, new Box2D( 0.20, -0.1, 0, 0.05, 0.01))
  //end
  goal( steps, new Box2D( 0.20, 0.09, 0, 0.05, 0.01))

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

object SymRun {

  def apply(test: String, args: McOptions) {
    val world = test match {
      case "1" => (() => new SymTest1(args.timeBound))
      case _ => (() => sys.error("unknown"))
    }
    val runner = new react.verification.McRunner(args, world)
    runner.bmc
  }

}
