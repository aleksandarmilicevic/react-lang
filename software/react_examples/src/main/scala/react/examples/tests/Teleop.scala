package react.examples.tests

import react._
import react.robot._
import react.message._
import react.examples._


class Teleop(_id: String) extends Robot(_id) {

  val period = 100

  val vLinMax: Byte = 6
  val vLinStep: Byte = 2
  val vAngMax: Byte = 6
  val vAngStep: Byte = 3
  def clamp(v: Int, vMin: Int, vMax: Int): Byte = math.min(vMax, math.max(v, vMin)).toByte

  //0.5 is ~1/2 size of the base along the x axis
  //0.8 is a guessed coefficient of friction
  //takes into account the fact that the controller runs every 100ms
  def safeDistance(v: Int) = 0.5 + v * v / (9.81 * 2.0 * 0.8) + vLinear * period / 1000.0

  //
  var vLinear: Byte = 0
  var vAngular: Byte = 0

  //input from keyboard
  on {
    case Key.UP =>    vLinear = clamp(vLinear + vLinStep, -vLinMax, vLinMax)
    case Key.DOWN =>  vLinear = clamp(vLinear - vLinStep, -vLinMax, vLinMax)
    case Key.LEFT =>  vAngular = clamp(vAngular + vAngStep, -vAngMax, vAngMax)
    case Key.RIGHT => vAngular = clamp(vAngular - vAngStep, -vAngMax, vAngMax)
    case Key.NONE =>  vLinear = 0; vAngular = 0
  }

  var frontDistance = 1.0f
  sensor[LaserScan]("laser"){
    case GetRange(distance) =>
      frontDistance = distance
  }
  
  every(period){
    if (frontDistance > safeDistance(vLinear) && vLinear > 0) {
      val cmd = Command.setSpeed(vLinear, vAngular/10.0)
      publish("cmd_vel", cmd)
    }
  }

}
