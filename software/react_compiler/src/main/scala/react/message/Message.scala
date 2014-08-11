package react.message

/* TODO generate all (most of) this using Macros */

case class Time(secs: Int, nsecs: Int)
case class Duration(secs: Int, nsecs: Int)

abstract class Message(val rosType: String) { }

case class Header(seq: Int, stamp: Time, frame_id: String) extends Message(std_msgs.Header._TYPE)

case class Pose2D(x: Double, y: Double, theta: Double) extends Message(geometry_msgs.Pose2D._TYPE)

case class Vector3(x: Double, y: Double, z: Double) extends Message(geometry_msgs.Vector3._TYPE)

case class Twist(linear: Vector3, angular: Vector3) extends Message(geometry_msgs.Twist._TYPE)

case class TwistStamped(header: Header, twist: Twist) extends Message(geometry_msgs.TwistStamped._TYPE)

case class Range(header: Header, kind: Byte, fov: Float, min: Float, max: Float, range: Float) extends Message(sensor_msgs.Range._TYPE)

case class Mvmt(header: Header, speed: Double, angular_speed: Double, d: Duration) extends Message(react_msgs.Mvmt._TYPE)

object Message {

  def time(ms: Long) = Time((ms/1000).toInt, ((ms % 1000) * 1000).toInt)
  def duration(ms: Long) = Duration((ms/1000).toInt, ((ms % 1000) * 1000).toInt)

  def fromDuration(d: org.ros.message.Duration) = Duration(d.secs, d.nsecs)

  def fromTime(t: org.ros.message.Time) = Time(t.secs, t.nsecs)

  def fromHeader(h: std_msgs.Header) = Header(h.getSeq, fromTime(h.getStamp), h.getFrameId)

  def fromVector3(v: geometry_msgs.Vector3) = Vector3(v.getX, v.getY, v.getZ)

  def fromPose2D(p: geometry_msgs.Pose2D) = Pose2D(p.getX, p.getY, p.getTheta)

  def fromTwist(t: geometry_msgs.Twist) = Twist(fromVector3(t.getLinear), fromVector3(t.getAngular))

  def fromTwistStamped(ts: geometry_msgs.TwistStamped) = TwistStamped(fromHeader(ts.getHeader), fromTwist(ts.getTwist))

  def fromRange(r: sensor_msgs.Range) = Range(fromHeader(r.getHeader),
                                              r.getRadiationType,
                                              r.getFieldOfView,
                                              r.getMinRange,
                                              r.getMaxRange,
                                              r.getRange)

  def fromMvmt(m: react_msgs.Mvmt) = Mvmt(fromHeader(m.getHeader), m.getSpeed, m.getAngularSpeed, fromDuration(m.getD))

  def fromMessage(rosType: String, msg: Any): Message = {
    if (rosType == std_msgs.Header._TYPE) fromHeader(msg.asInstanceOf[std_msgs.Header])
    else if (rosType == geometry_msgs.Pose2D._TYPE) fromPose2D(msg.asInstanceOf[geometry_msgs.Pose2D])
    else if (rosType == geometry_msgs.Vector3._TYPE) fromVector3(msg.asInstanceOf[geometry_msgs.Vector3])
    else if (rosType == geometry_msgs.Twist._TYPE) fromTwist(msg.asInstanceOf[geometry_msgs.Twist])
    else if (rosType == geometry_msgs.TwistStamped._TYPE) fromTwistStamped(msg.asInstanceOf[geometry_msgs.TwistStamped])
    else if (rosType == sensor_msgs.Range._TYPE) fromRange(msg.asInstanceOf[sensor_msgs.Range])
    else if (rosType == react_msgs.Mvmt._TYPE) fromMvmt(msg.asInstanceOf[react_msgs.Mvmt])
    else sys.error("TODO: message type " + rosType + " not yet supported")
  }

}

class MessageConverter(node: org.ros.node.Node) {

  def toDuration(d: Duration) = new org.ros.message.Duration(d.secs, d.nsecs)
  def toTime(t: Time) = new org.ros.message.Time(t.secs, t.nsecs)
  
  import org.ros.message._

  def toHeader(h: Header) = {
    val h2 = node.getTopicMessageFactory().newFromType[std_msgs.Header](h.rosType)
    h2.setSeq(h.seq)
    h2.setStamp(toTime(h.stamp))
    h2.setFrameId(h.frame_id)
    h2
  }

  def toPose2D(p: Pose2D) = {
    val p2 = node.getTopicMessageFactory().newFromType[geometry_msgs.Pose2D](p.rosType)
    p2.setX(p.x)
    p2.setY(p.y)
    p2.setTheta(p.theta)
    p2
  }
  
  def toVector3(v: Vector3) = {
    val v2 = node.getTopicMessageFactory().newFromType[geometry_msgs.Vector3](v.rosType)
    v2.setX(v.x)
    v2.setY(v.y)
    v2.setZ(v.z)
    v2
  }
  
  def toTwist(t: Twist) = {
    val t2 = node.getTopicMessageFactory().newFromType[geometry_msgs.Twist](t.rosType)
    t2.setLinear(toVector3(t.linear))
    t2.setAngular(toVector3(t.angular))
    t2
  }
  
  def toTwistStamped(ts: TwistStamped) = {
    val ts2 = node.getTopicMessageFactory().newFromType[geometry_msgs.TwistStamped](ts.rosType)
    ts2.setHeader(toHeader(ts.header))
    ts2.setTwist(toTwist(ts.twist))
    ts2
  }

  def toRange(r: Range) = {
    val r2 = node.getTopicMessageFactory().newFromType[sensor_msgs.Range](r.rosType)
    r2.setHeader(toHeader(r.header))
    r2.setRadiationType(r.kind)
    r2.setMinRange(r.min)
    r2.setMaxRange(r.max)
    r2.setRange(r.range)
  }

  def toMvmt(m: Mvmt) = {
    val m2 = node.getTopicMessageFactory().newFromType[react_msgs.Mvmt](m.rosType)
    m2.setHeader(toHeader(m.header))
    m2.setSpeed(m.speed)
    m2.setAngularSpeed(m.angular_speed)
    m2.setD(toDuration(m.d))
    m2
  }

}
