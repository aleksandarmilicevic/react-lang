package react.examples.bmcTest

import react.Robot
import react.message.Primitive.Int16
import react.verification._
import react.verification.model._
import react.verification.model.generic._
import react.verification.environment._
import react.verification.ghost._

class Arm(port: String, steps: Int) extends Robot(port) {

  assert(steps > 2)

  val max: Short =  45
  val min: Short = -45
  var counter = 0

  every(1) {
    if (counter == 0) {
      counter += 1
      publish("alpha", Int16( min ))
      publish("beta",  Int16(   0 ))
      publish("gamma", Int16(   0 ))
    } else if (counter == 1) {
      counter += 1
      publish("beta",  Int16( min ))
      publish("gamma", Int16(  90 )) //TODO I got that wrong
    } else if (counter > 1 && counter < steps - 1) {
      counter += 1
      val s = (max - min) / (steps - 3).toDouble 
      val a = min + (counter - 2) * s
      publish("alpha", Int16( a.toShort ))
    } else if (counter >= steps - 1) {
      counter += 1
      publish("beta",  Int16(   0 ))
      publish("gamma", Int16(   0 ))
    }
  }

}

abstract class ArmWorld(steps: Int) extends World {

  val xMin = -1
  val xMax = 1
  val xDiscretization = 0.015625 / 4 // 4

  val yMin = -1
  val yMax = 1
  val yDiscretization = 0.015625 / 4 // 4

  val enclosed = false
  
  val fpDiscretization = 0.015625
      
  //obstacle(new Box2D( 0.25, -0.1, 0, 0.2,  0.2))

  obstacle(new Box2D( 0.32, -0.1, 0, 0.2,  0.2)) //TODO circle outside the ...

  goal( 1, new Box2D( 0.20, -0.3, 0, 0.2, 0.2))
  goal( steps, new Box2D( 0.20,  0.1, 0, 0.2, 0.2))

  def safe = true
  

  val file: String
  val i = "/arm"

  def mkArm = {
    val m1 = GenericRobot(i, this, "verifier/src/test/resources/" + file) //the version with parametric segment
    m1.setPosition(0,0) //TODO get the right value
    m1.setOrientation(0)//TODO get the right value
    m1
  }

}

class Test1(steps: Int) extends ArmWorld(steps) {
  val file = "folded_arm.txt"
  robot(new Arm(i, steps), mkArm)
}

class Test2(steps: Int) extends ArmWorld(steps) {
  val file = "folded_arm_1param.txt"
  robot(new Arm(i, steps), mkArm)
}

class Test3(steps: Int) extends ArmWorld(steps) {
  val file = "folded_arm_3params.txt"
  robot(new Arm(i, steps), mkArm)
}


object Run {

  def apply(test: String, args: McOptions) {
    val world = test match {
      case "1" => (() => new Test1(args.timeBound))
      case "2" => (() => new Test2(args.timeBound))
      case "3" => (() => new Test3(args.timeBound))
      case _ => (() => sys.error("unknown"))
    }
    val runner = new react.verification.McRunner(args, world)
    runner.bmc(5)
  }

}
