package react.examples.husky

import react._
import react.message._
import react.examples._


class HuskyTeleop(_id: String) extends GroundRobot(_id) {

  var vx = 0
  var vy = 0

  //input from keyboard
  on {
    case Key.UP =>    vx += 1
    case Key.DOWN =>  vx -= 1
    case Key.LEFT =>  vy += 1
    case Key.RIGHT => vy -= 1
    case Key.NONE =>
  }
  //TODO clamp to v_min, v_max

  sensor[Odometry]("/odom"){
    case GetPose( pX, pY, pT) =>
      //update the position with the info from the robot
      x = pX
      y = pY
      theta = pT
  }
  
  //husky expect a message every 100ms
  every(100){
    publish("cmd_vel", SetSpeeds(vx, vy/10.0))
  }

}
