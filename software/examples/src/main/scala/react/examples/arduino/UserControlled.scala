package react.examples.arduino

import react._
import react.robot._
import react.message._
import react.examples._

class UserControlled(port: String) extends Robot(port) {

  val sensorDist   = "1" 
  val servoLeft    = "2"
  val servoRight   = "3" 

  var distance = 0
  sensor[Primitive.Int16](sensorDist){
    case Primitive.Int16(d) =>
      //Console.println("distance ← " + d)
      distance = d
  }

  //input from keyboard, just store the direction
  var dir = Key.DOWN
  on {
    case k: Key.Value => dir = k
  }

  
  val speed: Short = 80
  val half: Short = (speed/2).toShort
  val mhalf: Short = (-half).toShort
  val safeDistance: Short = 150
  every(200) {
    dir match {
      case Key.UP if (distance < safeDistance) =>
        publish(servoLeft, Primitive.Int16(speed))
        publish(servoRight, Primitive.Int16(speed))
      case Key.LEFT =>
        publish(servoLeft, Primitive.Int16(mhalf))
        publish(servoRight, Primitive.Int16(half))
      case Key.RIGHT =>
        publish(servoLeft, Primitive.Int16(half))
        publish(servoRight, Primitive.Int16(mhalf))
      case _ =>
        publish(servoLeft, Primitive.Int16(0))
        publish(servoRight, Primitive.Int16(0))
    }
  }

}
