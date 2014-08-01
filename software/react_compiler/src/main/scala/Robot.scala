package react

abstract class Robot {

  def on[T](handler: PartialFunction[T, Unit]) = {
    sys.error("TODO")
  }

  def every(period: Int)(body: () => Unit) = {
    sys.error("TODO")
  }

  //TODO the runtime stuff and the messages ...
  //connections
  //discovery of robot by react ?

}

abstract class GroundRobot extends Robot {

  //ROS Pose2D
  var x = 0.0
  var y = 0.0
  var theta = 0.0

}

//abstract class FlyingRobot extends Robot {
//  ROS Pose
//  var position: Vector3D
//  var orientation: Vector3D //actually should be a quaternion
//}
