package react.examples.tests

import react._
import react.robot._
import react.message._
import react.examples._
import react.examples.husky._

class Snappy(_id: String) extends GroundRobot(_id) {

  var poseUpdated = false
  var distanceUpdated = false

  sensor[Odometry]("p3d"){
    case GetPose(pX, pY, pT) =>
      //println("p3d")
      x = pX
      y = pY
      orientation = pT
      poseUpdated = true
  }

  var frontDistance = 1.0f
  sensor[LaserScan]("laser"){
    case GetRange(distance) =>
      //println("laser")
      frontDistance = distance.toFloat
      distanceUpdated = true
  }

  //input from keyboard
  on {
    case Key.UP =>
      //println("up")
      if (poseUpdated && distanceUpdated && frontDistance >= 2) {
        Orientation.closest(orientation) match {
          case North => y += 1
          case South => y -= 1
          case East  => x += 1
          case West  => x -= 1
        }
        snapTo(x, y, orientation)
      }
    case Key.LEFT  =>
      //println("left")
      if (poseUpdated) {
        Orientation.closest(orientation) match {
          case North => orientation = West.rad
          case South => orientation = East.rad
          case East  => orientation = North.rad
          case West  => orientation = South.rad
        }
        snapTo(x, y, orientation)
      }
    case Key.RIGHT =>
      //println("right")
      if (poseUpdated) {
        Orientation.closest(orientation) match {
          case North => orientation = East.rad
          case South => orientation = West.rad
          case East  => orientation = South.rad
          case West  => orientation = North.rad
        }
        snapTo(x, y, orientation)
      }
  }
  
  def snapTo(x: Double, y: Double, o: Double) = {
    val msg = Command.moveToAndOrient(id.substring(1), x, y, o)
    //println(msg)
    publish("/gazebo/set_model_state", msg)
    distanceUpdated = false
    poseUpdated = false
  }

}
