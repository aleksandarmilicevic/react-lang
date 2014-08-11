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
    case "Header" => std_msgs.Header._TYPE
    case "Pose2D" => geometry_msgs.Pose2D._TYPE
    case "Vector3" => geometry_msgs.Vector3._TYPE
    case "Twist" => geometry_msgs.Twist._TYPE
    case "TwistStamped" => geometry_msgs.TwistStamped._TYPE
    case "Range" => sensor_msgs.Range._TYPE
    case "Mvmt" => react_msgs.Mvmt._TYPE
    case other => sys.error("TODO: message type " + other + " not yet supported / unkown")
  }
  
  def convertMsgType(t: Type) = removeReactPrefix(t.toString) match {
    case "Header" => tq"std_msgs.Header"
    case "Pose2D" => tq"geometry_msgs.Pose2D"
    case "Vector3" => tq"geometry_msgs.Vector3"
    case "Twist" => tq"geometry_msgs.Twist"
    case "TwistStamped" => tq"geometry_msgs.TwistStamped"
    case "Range" => tq"sensor_msgs.Range"
    case "Mvmt" => tq"react_msgs.Mvmt"
    case other => sys.error("TODO: message type " + other + " not yet supported / unkown")
  }

  def convertMsgName(t: Type): c.Expr[String] = {
    val str = toRos(t.toString)
    c.Expr[String](Literal(Constant(str)))
  }

}
