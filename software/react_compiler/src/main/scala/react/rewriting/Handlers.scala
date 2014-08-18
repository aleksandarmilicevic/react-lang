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
    case "Header" =>                std_msgs.Header._TYPE
    case "Vector3" =>               geometry_msgs.Vector3._TYPE
    case "Point" =>                 geometry_msgs.Point._TYPE
    case "Quaternion" =>            geometry_msgs.Quaternion._TYPE
    case "Pose2D" =>                geometry_msgs.Pose2D._TYPE
    case "Pose" =>                  geometry_msgs.Pose._TYPE
    case "PoseStamped" =>           geometry_msgs.PoseStamped._TYPE
    case "PoseWithCovariance" =>    geometry_msgs.PoseWithCovariance._TYPE
    case "Twist" =>                 geometry_msgs.Twist._TYPE
    case "TwistStamped" =>          geometry_msgs.TwistStamped._TYPE
    case "TwistWithCovariance" =>   geometry_msgs.TwistWithCovariance._TYPE
    case "Range" =>                 sensor_msgs.Range._TYPE
    case "LaserScan" =>             sensor_msgs.LaserScan._TYPE
    case "Imu" =>                   sensor_msgs.Imu._TYPE
    case "Odometry" =>              nav_msgs.Odometry._TYPE
    case "Path" =>                  nav_msgs.Path._TYPE
    case "Mvmt" =>                  react_msgs.Mvmt._TYPE
    case other => sys.error("TODO: message type " + other + " not yet supported / unkown")
  }
  
  def convertMsgType(t: Type) = removeReactPrefix(t.toString) match {
    case "Header" =>                tq"std_msgs.Header"
    case "Vector3" =>               tq"geometry_msgs.Vector3"
    case "Point" =>                 tq"geometry_msgs.Point"
    case "Quaternion" =>            tq"geometry_msgs.Quaternion"
    case "Pose2D" =>                tq"geometry_msgs.Pose2D"
    case "Pose" =>                  tq"geometry_msgs.Pose"
    case "PoseStamped" =>           tq"geometry_msgs.PoseStamped"
    case "PoseWithCovariance" =>    tq"geometry_msgs.PoseWithCovariance"
    case "Twist" =>                 tq"geometry_msgs.Twist"
    case "TwistStamped" =>          tq"geometry_msgs.TwistStamped"
    case "TwistWithCovariance" =>   tq"geometry_msgs.TwistWithCovariance"
    case "Range" =>                 tq"sensor_msgs.Range"
    case "LaserScan" =>             tq"sensor_msgs.LaserScan"
    case "Imu" =>                   tq"sensor_msgs.Imu"
    case "Odometry" =>              tq"nav_msgs.Odometry"
    case "Path" =>                  tq"nav_msgs.Path"
    case "Mvmt" =>                  tq"react_msgs.Mvmt"
    case other => sys.error("TODO: message type " + other + " not yet supported / unkown")
  }

  def convertMsgName(t: Type): c.Expr[String] = {
    val str = toRos(t.toString)
    c.Expr[String](Literal(Constant(str)))
  }

}
