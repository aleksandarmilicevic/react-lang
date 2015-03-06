package react.examples.arduino

import react._
import react.robot._
import react.message._
import react.utils.Env
import react.examples._
import react.runtime.{Arduino, ArduinoExecutor}

class FollowTheEdge(port: String, clockwise: Boolean = false) extends Robot(port) {

  val sensor     = "2"
  val servoLeft  = "32"
  val servoRight = "9"

  var onTarget = false
  sensor[Primitive.Bool](sensor){
    case Primitive.Bool(b) =>
      Console.println("onTarget ← " + b)
      onTarget = b
  }

  val defaultSpeed: Short = -15
  //val defaultSpeed: Short = 78 // 2.44 rad/s in the MC
  var lSpeed: Short = Env.getShort("lSpeed", defaultSpeed)
  var rSpeed: Short = Env.getShort("rSpeed", defaultSpeed)

  var currDirection = !clockwise

  every(100) {
    val newDir = onTarget ^ clockwise
    if (newDir != currDirection) {
      if (newDir) {
        //Console.println("turning right")
        publish(servoLeft, Primitive.Int16(lSpeed))
        publish(servoRight, Primitive.Int16(0))
      } else {
        //Console.println("turning servoLeft")
        publish(servoLeft, Primitive.Int16(0))
        publish(servoRight, Primitive.Int16(rSpeed))
      }
      currDirection = newDir
    }
  }

}

