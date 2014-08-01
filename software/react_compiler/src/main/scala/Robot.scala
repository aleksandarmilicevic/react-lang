package react

abstract class Robot {

  //TODO what about the orientation ?

  def on[T](handler: PartialFunction[T, Unit]) = {
    sys.error("TODO")
  }

  def every(period: Int)(body: PartialFunction[Unit, Unit]) = {
    sys.error("TODO")
  }

  //TODO the runtime stuff and the messages ...
  //connections
  //discovery of robot by react ?

}

//abstract class GroundRobot extends Robot {
//  //ROS Pose2D
//  var x: Double
//  var y: Double
//  var theta: Double
//}

//abstract class FlyingRobot extends Robot {
//  ROS Pose
//  var position: Vector3D
//  var orientation: Vector3D //actually should be a quaternion
//}
