package react.examples.arduino

import react._
import react.robot._
import react.message._
import react.examples._
import react.runtime.{Arduino, ArduinoExecutor}

class FollowTheEdge(port: String, clockwise: Boolean = false) extends Robot(port) {

  val sensor     = "16"
  val servoLeft  = "10"
  val servoRight = "4"

  var onTarget = false
//sensor[Primitive.Bool](sensor){
//  case Primitive.Bool(b) =>
//    //Console.println("onTarget ← " + b)
//    onTarget = b
//}
  sensor[Primitive.Int16](sensor){
    case Primitive.Int16(i) =>
      Console.println("onTarget ← " + i)
      onTarget = (i < 450)
  }
  
  val speed: Short = -25

  var currDirection = !clockwise

  every(100) {
    val newDir = onTarget ^ clockwise
    if (newDir != currDirection) {
      if (newDir) {
        //Console.println("turning right")
        publish(servoLeft, Primitive.Int16(speed))
        publish(servoRight, Primitive.Int16(0))
      } else {
        //Console.println("turning servoLeft")
        publish(servoLeft, Primitive.Int16(0))
        publish(servoRight, Primitive.Int16(speed))
      }
      currDirection = newDir
    }
  }

}

