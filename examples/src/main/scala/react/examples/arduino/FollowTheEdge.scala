package react.examples.arduino

import react.Robot
import react.message.Primitive.{Int16,Bool}
import react.utils.Env

class FollowTheEdge(port: String, clockwise: Boolean = false) extends Robot(port) {

  val sensor     = "2"
  val servoLeft  = "32"
  val servoRight = "9"

  var onTarget = false

  sensor[Bool](sensor){
    case Bool(b) =>
      //Console.println("onTarget ← " + b)
      onTarget = b
  }

  //val defaultSpeed: Short = 5
  val defaultSpeed: Short = 78 // 2.44 rad/s in the MC
  val lSpeed: Short = Env.getShort("lSpeed", defaultSpeed)
  val rSpeed: Short = Env.getShort("rSpeed", defaultSpeed)

  every(100) {
    val newDir = onTarget ^ clockwise
    if (newDir) {
      //Console.println("turning right")
      publish(servoLeft,  Int16(lSpeed))
      publish(servoRight, Int16(0))
    } else {
      //Console.println("turning left")
      publish(servoLeft,  Int16(0))
      publish(servoRight, Int16(rSpeed))
    }
  }

}

