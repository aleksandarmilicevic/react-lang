package react.examples.husky

import react._
import react.robot._
import react.message._
import react.examples._


class HuskyTeleop(_id: String) extends GroundRobot(_id) {

  val period = 100

  val vLinMax = 6
  val vLinStep = 2
  val vAngMax = 6
  val vAngStep = 6
  def clamp(v: Int, vMin: Int, vMax: Int) = math.min(vMax, math.max(v, vMin))
  //0.5 is ~1/2 size of the base along the x axis
  //0.8 is a guessed coefficient of friction
  //takes into account the fact that the controller runs every 100ms
  def safeDistance(v: Int) = 0.5 + v * v / (9.81 * 2.0 * 0.8) + vLinear * period / 1000.0

  //
  var vLinear = 0
  var vAngular = 0

  //input from keyboard
  on {
    case Key.UP =>    vLinear = clamp(vLinear + vLinStep, -vLinMax, vLinMax)
    case Key.DOWN =>  vLinear = clamp(vLinear - vLinStep, -vLinMax, vLinMax)
    case Key.LEFT =>  vAngular = clamp(vAngular + vAngStep, -vAngMax, vAngMax)
    case Key.RIGHT => vAngular = clamp(vAngular - vAngStep, -vAngMax, vAngMax)
    case Key.NONE =>  vLinear = 0; vAngular = 0
  }

  sensor[Odometry]("p3d"){
    case GetPose( pX, pY, pT) =>
      //update the position with the info from the robot
      x = pX
      y = pY
      orientation = pT
  }

  sensor[LaserScan]("laser"){
    case GetRange(distance) => 
      if (distance <= safeDistance(vLinear) && vLinear > 0) {
        vLinear = 0
        vAngular = 0
      }
  }
  
  //husky expect a message every 100ms
  every(period){
    publish("husky/cmd_vel", Command.setSpeed(vLinear, vAngular/10.0))
  }

}
