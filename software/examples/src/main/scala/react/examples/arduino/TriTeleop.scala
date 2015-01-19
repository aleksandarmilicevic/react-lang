package react.examples.arduino

import react._
import react.robot._
import react.message._
import react.utils.Env
import react.examples._
import react.runtime.{Arduino, ArduinoExecutor}

class TriTeleop(port: String) extends Robot(port) {

  //port
  val sensorDist    = "33" 
  val steeringServo = "16"
  val motorServo    = "4"

  //var
  var speed = 0
  var angle = 0
  var speedChanged = true
  var angleChanged = true

  //user input
  on {
    case Key.UP =>    speed += 1; Console.println("speed: " + speed); speedChanged = true
    case Key.DOWN =>  speed -= 1; Console.println("speed: " + speed); speedChanged = true
    case Key.LEFT =>  angle += 1; Console.println("angle: " + angle); angleChanged = true
    case Key.RIGHT => angle -= 1; Console.println("angle: " + angle); angleChanged = true
  }
  
  //pushing the values
  every(100) {
    if (speedChanged) {
      publish(motorServo, Primitive.Int16(speed.toShort))
      speedChanged = false
    }
    if (angleChanged) {
      publish(steeringServo, Primitive.Int16(angle.toShort))
      angleChanged = false
    }
  }
  
  sensor[Primitive.Int16](sensorDist){
    case Primitive.Int16(i) =>
      Console.println("distance: " + i)
  }

}
