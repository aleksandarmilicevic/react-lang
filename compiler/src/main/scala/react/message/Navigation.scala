package react.message

//nav_msgs
case class Odometry(header: Header, childFrameId: String, pose: PoseWithCovariance, twist: TwistWithCovariance) extends Message(nav_msgs.Odometry._TYPE)
case class Path(header: Header, poses: Array[PoseStamped]) extends Message(nav_msgs.Path._TYPE)

