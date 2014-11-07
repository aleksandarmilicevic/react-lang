package react.examples.arduino

import react._
import react.robot._
import react.message._
import react.examples._
import react.runtime.{Arduino, ArduinoExecutor}

class FollowTheEdge(port: String, clockwise: Boolean = false) extends Robot(port) {

  val sensor     = Arduino.D2
  val servoLeft  = Arduino.D9
  val servoRight = Arduino.D10

  var onTarget = false
  sensor[Primitive.Bool](sensor){
    case Primitive.Bool(b) =>
      //Console.println("onTarget ← " + b)
      onTarget = b
  }
  
  val speed: Short = 80
  every(100) {
    if (onTarget ^ clockwise) {
      //Console.println("turning right")
      publish(servoLeft, Primitive.Int16(speed))
      publish(servoRight, Primitive.Int16(0))
    } else {
      //Console.println("turning servoLeft")
      publish(servoLeft, Primitive.Int16(0))
      publish(servoRight, Primitive.Int16(speed))
    }
  }

}

