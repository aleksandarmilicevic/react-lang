package react.examples.arduino

import react._
import react.robot._
import react.message._
import react.utils.Env
import react.examples._
import react.runtime.{Arduino, ArduinoExecutor}

class FollowTheEdge(port: String, clockwise: Boolean = false) extends Robot(port) {

  val sensor     = "2"
  val servoLeft  = "15"
  val servoRight = "5"

  var onTarget = false
  sensor[Primitive.Bool](sensor){
    case Primitive.Bool(b) =>
      Console.println("onTarget ← " + b)
      onTarget = b
  }
  //sensor[Primitive.Int16](sensor){
  //  case Primitive.Int16(i) =>
  //    Console.println("onTarget ← " + i)
  //    onTarget = (i < 450)
  //}
  
  //for calibration
//on {
//  case Key.UP =>
//    lSpeed = (lSpeed + 1).toShort
//    publish(servoLeft, Primitive.Int16(lSpeed))
//  case Key.DOWN =>
//    lSpeed = (lSpeed - 1).toShort
//    publish(servoLeft, Primitive.Int16(lSpeed))
//  case Key.LEFT =>
//    rSpeed = (rSpeed + 1).toShort
//    publish(servoRight, Primitive.Int16(rSpeed))
//  case Key.RIGHT =>
//    rSpeed = (rSpeed - 1).toShort
//    publish(servoRight, Primitive.Int16(rSpeed))
//}

  //val defaultSpeed: Short = 8 // 78 for 2.44 rad/s in the MC
  val defaultSpeed: Short = 78 // 2.44 rad/s in the MC
  var lSpeed: Short = Env.getShort("lSpeed", defaultSpeed)
  var rSpeed: Short = Env.getShort("rSpeed", defaultSpeed)
  //what we got last time after calibration
//var lSpeed: Short = -7
//var rSpeed: Short =  8

  var currDirection = !clockwise

  every(100) {
    val newDir = onTarget ^ clockwise
  //if (newDir != currDirection) {
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
  //}
  }

}
