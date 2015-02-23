package react.robot

import react._
import react.message._

object Command {
  
  def setSpeed(linear: Double, angular: Double): Twist = {
    Twist(Vector3(linear,0,0), Vector3(0,0, angular))
  }

  def moveTo(name: String, x: Double, y: Double): ModelState = {
    ModelState(name, Pose(Point(x, y, 0), Quaternion(0, 0, 0, 0)), setSpeed(0, 0), "world")
  }

  def moveToAndOrient(name: String, x: Double, y: Double, o: Double): ModelState = {
    val q = Angle.quaternionFromTheta(o)
    ModelState(name, Pose(Point(x, y, 0), q), setSpeed(0, 0), "world")
  }

}
