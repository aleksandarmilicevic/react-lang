package react.examples.husky

import react._
import react.robot._
import react.message._
import react.examples._


class HuskyGrid2(_id: String) extends GroundRobot(_id) {

  val period = 100

  def clamp(v: Int, vMin: Int, vMax: Int) = math.min(vMax, math.max(v, vMin))

  //input from keyboard
  on {
    case Key.UP    => y += 1.0; publishPos()
    case Key.DOWN  => y -= 1.0; publishPos()
    case Key.LEFT  => x -= 1.0; publishPos()
    case Key.RIGHT => x += 1.0; publishPos()
    case Key.NONE  => x = 0.0; y = 0.0; publishPos()
  }

  // sensor[Odometry]("p3d"){
  //   case GetPose( pX, pY, pT) =>
  //     //update the position with the info from the robot
  //     x = pX
  //     y = pY
  //     orientation = pT
  // }

  sensor[LaserScan]("laser"){
    case GetRange(distance) => 
      // println("distance = " + distance)
      if (distance <= 2) {
        println("too close")
      }
  }
  
  // //husky expect a message every 100ms
  // every(period){
  //   // publish("husky/cmd_vel", Command.setSpeed(vLinear, vAngular/10.0))
  //   var modelName = _id.substring(1)
  //   println("publishing coords for " + modelName)
  //   println("x = " + x)
  //   println("y = " + y)
  //   publish("gazebo/set_model_state", Command.moveTo(modelName, x, y))
  // }

  def publishPos() = {
    var modelName = _id.substring(1)
    println("publishing coords for " + modelName)
    println("x = " + x)
    println("y = " + y)
    publish("gazebo/set_model_state", Command.moveTo(modelName, x, y))
  }

}
