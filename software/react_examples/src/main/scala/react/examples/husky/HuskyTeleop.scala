package react.examples.husky

import react._
import react.message._
import react.examples._


class HuskyTeleop(_id: String) extends GroundRobot(_id) {

  var vx = 0
  var vy = 0

  //input from keyboard
  on {
    case Key.UP =>    vy += 1
    case Key.DOWN =>  vy -= 1
    case Key.LEFT =>  vx += 1
    case Key.RIGHT => vx -= 1
    case Key.NONE =>
  }
  //TODO clamp to v_min, v_max

  sensor[Odometry]("/odom"){
    case Odometry(_, _, PoseWithCovariance(Pose(Point(oX,oY,_), Quaternion(_,_,qZ,qW)),_), _) =>
      //update the position of the robot according to what the robot says
      x = oX
      y = oY
      theta = 2*math.acos(qW)*math.signum(qZ) //TODO how do we get the rotation back form the quaterion ?
    case other => sys.error("message did not match: " + other)
  }
  
  //husky takes a message every 100ms
  every(100){
    val targetSpeed = math.sqrt(vx*vx + vy*vy)
    val targetTheta = if (vx != 0 || vy != 0) math.atan2(vy, vx) else theta
    val rot = targetTheta - theta
    publish("cmd_vel", Twist(Vector3(targetSpeed,0,0), Vector3(0,0, rot)))
    println("x = " + x)
    println("y = " + y)
    println("θ = " + theta)
    println("vx = " + vx)
    println("vy = " + vy)
    println("tθ = " + targetTheta)
  }

}
