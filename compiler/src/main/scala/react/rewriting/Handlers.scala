package react.rewriting

trait Handlers {
  self: RobotMacros =>
  import c.universe._

  /* TODO generate all (most of) this using Macros */
  
  def removeReactPrefix(name: String): String = {
    val prefix = "react.message."
    if (name startsWith prefix) {
      name.substring(prefix.length, name.length)
    } else {
      name
    }
  }

  def toRos(name: String) = removeReactPrefix(name) match {
    case "Header"              => std_msgs.Header._TYPE
    case "Vector3"             => geometry_msgs.Vector3._TYPE
    case "Point"               => geometry_msgs.Point._TYPE
    case "Quaternion"          => geometry_msgs.Quaternion._TYPE
    case "Pose2D"              => geometry_msgs.Pose2D._TYPE
    case "Pose"                => geometry_msgs.Pose._TYPE
    case "PoseStamped"         => geometry_msgs.PoseStamped._TYPE
    case "PoseWithCovariance"  => geometry_msgs.PoseWithCovariance._TYPE
    case "Twist"               => geometry_msgs.Twist._TYPE
    case "TwistStamped"        => geometry_msgs.TwistStamped._TYPE
    case "TwistWithCovariance" => geometry_msgs.TwistWithCovariance._TYPE
    case "Range"               => sensor_msgs.Range._TYPE
    case "LaserScan"           => sensor_msgs.LaserScan._TYPE
    case "Imu"                 => sensor_msgs.Imu._TYPE
    case "Odometry"            => nav_msgs.Odometry._TYPE
    case "Path"                => nav_msgs.Path._TYPE
    case "Mvmt"                => react_msgs.Mvmt._TYPE
    case "ModelState"          => gazebo_msgs.ModelState._TYPE
    case "Primitive.Empty"     => std_msgs.Empty._TYPE
    case "Primitive.Bool"      => std_msgs.Bool._TYPE
    case "Primitive.Byte"      => std_msgs.Byte._TYPE
    case "Primitive.Char"      => std_msgs.Char._TYPE
    case "Primitive.Int16"     => std_msgs.Int16._TYPE
    case "Primitive.Int32"     => std_msgs.Int32._TYPE
    case "Primitive.Int64"     => std_msgs.Int64._TYPE
    case "Primitive.Float32"   => std_msgs.Float32._TYPE
    case "Primitive.Float64"   => std_msgs.Float64._TYPE
    case "Primitive.String"    => std_msgs.String._TYPE
    case "Primitive.Duration"  => std_msgs.Duration._TYPE
    case "Primitive.Time"      => std_msgs.Time._TYPE
    case other => sys.error("TODO: message type " + other + " not yet supported / unkown")
  }
  
  def convertMsgType(t: Type) = removeReactPrefix(t.toString) match {
    case "Header"              => tq"std_msgs.Header"
    case "Vector3"             => tq"geometry_msgs.Vector3"
    case "Point"               => tq"geometry_msgs.Point"
    case "Quaternion"          => tq"geometry_msgs.Quaternion"
    case "Pose2D"              => tq"geometry_msgs.Pose2D"
    case "Pose"                => tq"geometry_msgs.Pose"
    case "PoseStamped"         => tq"geometry_msgs.PoseStamped"
    case "PoseWithCovariance"  => tq"geometry_msgs.PoseWithCovariance"
    case "Twist"               => tq"geometry_msgs.Twist"
    case "TwistStamped"        => tq"geometry_msgs.TwistStamped"
    case "TwistWithCovariance" => tq"geometry_msgs.TwistWithCovariance"
    case "Range"               => tq"sensor_msgs.Range"
    case "LaserScan"           => tq"sensor_msgs.LaserScan"
    case "Imu"                 => tq"sensor_msgs.Imu"
    case "Odometry"            => tq"nav_msgs.Odometry"
    case "Path"                => tq"nav_msgs.Path"
    case "Mvmt"                => tq"react_msgs.Mvmt"
    case "ModelState"          => tq"gazebo_msgs.ModelState"
    case "Primitive.Empty" | "Primitive.Empty.type" => tq"std_msgs.Empty"
    case "Primitive.Bool"      => tq"std_msgs.Bool"
    case "Primitive.Byte"      => tq"std_msgs.Byte"
    case "Primitive.Char"      => tq"std_msgs.Char"
    case "Primitive.Int16"     => tq"std_msgs.Int16"
    case "Primitive.Int32"     => tq"std_msgs.Int32"
    case "Primitive.Int64"     => tq"std_msgs.Int64"
    case "Primitive.Float32"   => tq"std_msgs.Float32"
    case "Primitive.Float64"   => tq"std_msgs.Float64"
    case "Primitive.String"    => tq"std_msgs.String"
    case "Primitive.Duration"  => tq"std_msgs.Duration"
    case "Primitive.Time"      => tq"std_msgs.Time"
    case other => sys.error("TODO: message type " + other + " not yet supported / unkown")
  }

  def convertMsgName(t: Type): c.Expr[String] = {
    val str = toRos(t.toString)
    c.Expr[String](Literal(Constant(str)))
  }

}
