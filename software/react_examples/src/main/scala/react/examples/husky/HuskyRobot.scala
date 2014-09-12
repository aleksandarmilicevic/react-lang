package react.examples.husky

import react._
import react.robot._
import react.message._
import react.examples._

class HuskyRobot(_id: String) extends GroundRobot(_id) {

  var targetX = 0.0
  var targetY = 0.0
  var targetO = 0.0

  var frontDistance = 1.0

  def currentIntegralPosition = {
    val pX = math.round(x)
    val pY = math.round(y)
    val pO = Orientation.closest(orientation)
    (pX, pY, pO)
  }

  def modelName() = {
    _id.substring(1)
  }

  def closeEnough(dx: Double, dy: Double) = {
    val xx = Math.abs(dx)
    val yy = Math.abs(dy)
    val d = Math.sqrt(xx*xx + yy+yy)
    d < 0.2
  }

  def snap() = {
    publish("/gazebo/set_model_state", 
      Command.moveToAndOrient(modelName(), targetX, targetY, targetO))
  }

  //update the position with the info from the robot
  sensor[Odometry]("p3d"){
    case GetPose(pX, pY, pT) =>
      x = pX
      y = pY
      orientation = pT
  }

  sensor[LaserScan]("laser"){
    case GetRange(distance) =>
      println("distance = " + distance)
      frontDistance = distance
  }


}
