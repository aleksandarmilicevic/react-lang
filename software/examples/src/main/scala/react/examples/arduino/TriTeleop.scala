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

  val maxSpeed = 50
  val maxAngle = 70
  val speedOffset = -5

  //user input
  on {
    case Key.UP =>
      speed = math.min(speed + 1, maxSpeed)
      Console.println("speed: " + speed)
      speedChanged = true
    case Key.DOWN =>
      speed = math.max(speed - 1, -maxSpeed)
      Console.println("speed: " + speed)
      speedChanged = true
    case Key.LEFT =>
      angle = math.min(angle + 1, maxAngle)
      Console.println("angle: " + angle)
      angleChanged = true
    case Key.RIGHT =>
      angle = math.max(angle - 1, -maxAngle)
      Console.println("angle: " + angle)
      angleChanged = true
  }

  //bring things back to 0
  every(100) {
    if (speed > 0) { speed -= 1; speedChanged = true }
    else if (speed < 0) { speed += 1; speedChanged = true }
    if (angle > 0) { angle -= 1; angleChanged = true }
    else if (angle < 0) { angle += 1; angleChanged = true }
  }
  
  //pushing the values
  every(100) {
    if (speedChanged) {
      publish(motorServo, Primitive.Int16((speed + speedOffset).toShort))
      speedChanged = false
    }
    if (angleChanged) {
      publish(steeringServo, Primitive.Int16((-angle).toShort))
      angleChanged = false
    }
  }
  
  sensor[Primitive.Int16](sensorDist){
    case Primitive.Int16(i) =>
      Console.println("distance: " + i)
  }

}
