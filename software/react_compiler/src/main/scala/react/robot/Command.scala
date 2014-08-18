package react.robot

import react._
import react.message._

object Command {
  
  def setSpeed(linear: Double, angular: Double): Twist = {
    Twist(Vector3(linear,0,0), Vector3(0,0, angular))
  }

}
