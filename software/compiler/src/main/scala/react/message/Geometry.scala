package react.message

//from geometry_msgs

case class Vector3(x: Double, y: Double, z: Double) extends Message(geometry_msgs.Vector3._TYPE)
case class Point(x: Double, y: Double, z: Double) extends Message(geometry_msgs.Point._TYPE)
case class Quaternion(x: Double, y: Double, z: Double, w: Double) extends Message(geometry_msgs.Quaternion._TYPE)
case class Pose2D(x: Double, y: Double, theta: Double) extends Message(geometry_msgs.Pose2D._TYPE)
case class Pose(position: Point, orientation: Quaternion) extends Message(geometry_msgs.Pose._TYPE)
case class PoseStamped(header: Header, pose: Pose) extends Message(geometry_msgs.PoseStamped._TYPE)
case class PoseWithCovariance(pose: Pose, covariance: Array[Double]) extends Message(geometry_msgs.PoseWithCovariance._TYPE) {
  assert(covariance.size == 36, "PoseWithCovariance.covariance must contain 36 elements")
}
case class Twist(linear: Vector3, angular: Vector3) extends Message(geometry_msgs.Twist._TYPE)
case class TwistStamped(header: Header, twist: Twist) extends Message(geometry_msgs.TwistStamped._TYPE)
case class TwistWithCovariance(twist: Twist, covariance: Array[Double]) extends Message(geometry_msgs.TwistWithCovariance._TYPE) {
  assert(covariance.size == 36, "TwistWithCovariance.covariance must contain 36 elements")
}

