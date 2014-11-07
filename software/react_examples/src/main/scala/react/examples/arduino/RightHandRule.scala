package react.examples.arduino

import react._
import react.robot._
import react.message._
import react.examples._

class RightHandRule(port: String) extends Robot(port) {

  val sensorTarget = "0"
  val sensorDist   = "1" 
  val servoLeft    = "2"
  val servoRight   = "3" 

  var distance = 0
  sensor[Primitive.Int16](sensorDist){
    case Primitive.Int16(d) =>
      //Console.println("distance ← " + d)
      distance = d
  }

  var onTarget = false
  sensor[Primitive.Bool](sensorTarget){
    case Primitive.Bool(b) =>
      //Console.println("onTarget ← " + b)
      onTarget = b
  }

  
  val speed: Short = 80
  val half: Short = (speed/2).toShort
  val mhalf: Short = (-half).toShort
  val safeDistance: Short = 150
  every(200) {
    if (onTarget) {
      //Console.println("stopped")
      publish(servoLeft, Primitive.Int16(0))
      publish(servoRight, Primitive.Int16(0))
    } else if (distance < safeDistance) { //sensor reading is inversely proportional
      //Console.println("straight")
      publish(servoLeft, Primitive.Int16(speed))
      publish(servoRight, Primitive.Int16(speed))
    } else {
      //Console.println("right")
      publish(servoLeft, Primitive.Int16(half))
      publish(servoRight, Primitive.Int16(mhalf))
    }
  }

}
