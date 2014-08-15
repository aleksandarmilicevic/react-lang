package react.message

/* TODO automatically generate all (most of) this (using Macros)
 * adding new messages also requires fiiling some type info in react.rewriting.Handlers
 */

case class Time(secs: Int, nsecs: Int)
case class Duration(secs: Int, nsecs: Int)

abstract class Message(val rosType: String) { }

//std_msgs
case class Header(seq: Int, stamp: Time, frame_id: String) extends Message(std_msgs.Header._TYPE)

//geometry_msgs
case class Vector3(x: Double, y: Double, z: Double) extends Message(geometry_msgs.Vector3._TYPE)
case class Point(x: Double, y: Double, z: Double) extends Message(geometry_msgs.Point._TYPE)
case class Quaternion(x: Double, y: Double, z: Double, w: Double) extends Message(geometry_msgs.Quaternion._TYPE)
case class Pose2D(x: Double, y: Double, theta: Double) extends Message(geometry_msgs.Pose2D._TYPE)
case class Pose(position: Point, orientation: Quaternion) extends Message(geometry_msgs.Pose._TYPE)
case class PoseWithCovariance(pose: Pose, covariance: Array[Double]) extends Message(geometry_msgs.PoseWithCovariance._TYPE) {
  assert(covariance.size == 36, "PoseWithCovariance.covariance must contain 36 elements")
}
case class Twist(linear: Vector3, angular: Vector3) extends Message(geometry_msgs.Twist._TYPE)
case class TwistStamped(header: Header, twist: Twist) extends Message(geometry_msgs.TwistStamped._TYPE)
case class TwistWithCovariance(twist: Twist, covariance: Array[Double]) extends Message(geometry_msgs.TwistWithCovariance._TYPE) {
  assert(covariance.size == 36, "TwistWithCovariance.covariance must contain 36 elements")
}

//sensor_msgs
case class Range(header: Header, kind: Byte, fov: Float, min: Float, max: Float, range: Float) extends Message(sensor_msgs.Range._TYPE)
case class LaserScan( header: Header,
                      angleMin: Float,
                      angleMax: Float,
                      angleIncrement: Float,
                      timeIncrement: Float,
                      scanTime: Float,
                      rangeMin: Float,
                      rangeMax: Float,
                      ranges: Array[Float],
                      intensities: Array[Float]) extends Message(sensor_msgs.LaserScan._TYPE)
case class Imu( header: Header,
                orientation: Quaternion,
                orientationCovariance: Array[Double], //9
                angularVelocity: Vector3,
                angularVelocityCovariance: Array[Double], //9
                linearAcceleration: Vector3,
                linearAccelerationCovariance: Array[Double] //9
            ) extends Message(sensor_msgs.Imu._TYPE) {
  assert(orientationCovariance.size == 9, "Imu.orientationCovariance must contain 9 elements")
  assert(angularVelocityCovariance.size == 9, "Imu.angularVelocityCovariance must contain 9 elements")
  assert(linearAccelerationCovariance.size == 9, "Imu.linearAccelerationCovariance must contain 9 elements")
}

//nav_msgs
case class Odometry(header: Header, childFrameId: String, pose: PoseWithCovariance, twist: TwistWithCovariance) extends Message(nav_msgs.Odometry._TYPE)

//react_msgs
case class Mvmt(header: Header, speed: Double, angular_speed: Double, d: Duration) extends Message(react_msgs.Mvmt._TYPE)

object Message {

  def time(ms: Long) = Time((ms/1000).toInt, ((ms % 1000) * 1000).toInt)
  def duration(ms: Long) = Duration((ms/1000).toInt, ((ms % 1000) * 1000).toInt)

  def from(d: org.ros.message.Duration): Duration = Duration(d.secs, d.nsecs)
  def from(t: org.ros.message.Time): Time = Time(t.secs, t.nsecs)

  def from(h: std_msgs.Header): Header = Header(h.getSeq, from(h.getStamp), h.getFrameId)

  def from(v: geometry_msgs.Point): Point = Point(v.getX, v.getY, v.getZ)
  def from(v: geometry_msgs.Vector3): Vector3 = Vector3(v.getX, v.getY, v.getZ)
  def from(v: geometry_msgs.Quaternion): Quaternion = Quaternion(v.getX, v.getY, v.getZ, v.getW)
  def from(p: geometry_msgs.Pose2D): Pose2D = Pose2D(p.getX, p.getY, p.getTheta)
  def from(p: geometry_msgs.Pose): Pose = Pose(from(p.getPosition), from(p.getOrientation))
  def from(p: geometry_msgs.PoseWithCovariance): PoseWithCovariance = PoseWithCovariance(from(p.getPose), p.getCovariance)
  def from(t: geometry_msgs.Twist): Twist = Twist(from(t.getLinear), from(t.getAngular))
  def from(t: geometry_msgs.TwistStamped): TwistStamped = TwistStamped(from(t.getHeader), from(t.getTwist))
  def from(t: geometry_msgs.TwistWithCovariance): TwistWithCovariance = TwistWithCovariance(from(t.getTwist), t.getCovariance)

  def from(r: sensor_msgs.Range): Range = Range(from(r.getHeader),
                                                r.getRadiationType,
                                                r.getFieldOfView,
                                                r.getMinRange,
                                                r.getMaxRange,
                                                r.getRange)
  def from(r: sensor_msgs.LaserScan): LaserScan = LaserScan(from(r.getHeader),
                                                            r.getAngleMin,
                                                            r.getAngleMax,
                                                            r.getAngleIncrement,
                                                            r.getTimeIncrement,
                                                            r.getScanTime,
                                                            r.getRangeMin,
                                                            r.getRangeMax,
                                                            r.getRanges,
                                                            r.getIntensities)
  def from(r: sensor_msgs.Imu): Imu = Imu(from(r.getHeader),
                                          from(r.getOrientation),
                                          r.getOrientationCovariance,
                                          from(r.getAngularVelocity),
                                          r.getAngularVelocityCovariance,
                                          from(r.getLinearAcceleration),
                                          r.getLinearAccelerationCovariance)

  def from(o: nav_msgs.Odometry): Odometry = Odometry(from(o.getHeader),
                                                      o.getChildFrameId, 
                                                      from(o.getPose),
                                                      from(o.getTwist))

  def from(m: react_msgs.Mvmt): Mvmt = Mvmt(from(m.getHeader), m.getSpeed, m.getAngularSpeed, from(m.getD))

  def fromMessage(rosType: String, msg: Any): Message = {
    if (rosType == std_msgs.Header._TYPE) from(msg.asInstanceOf[std_msgs.Header])
    else if (rosType == geometry_msgs.Point._TYPE) from(msg.asInstanceOf[geometry_msgs.Point])
    else if (rosType == geometry_msgs.Vector3._TYPE) from(msg.asInstanceOf[geometry_msgs.Vector3])
    else if (rosType == geometry_msgs.Quaternion._TYPE) from(msg.asInstanceOf[geometry_msgs.Quaternion])
    else if (rosType == geometry_msgs.Pose2D._TYPE) from(msg.asInstanceOf[geometry_msgs.Pose2D])
    else if (rosType == geometry_msgs.Pose._TYPE) from(msg.asInstanceOf[geometry_msgs.Pose])
    else if (rosType == geometry_msgs.PoseWithCovariance._TYPE) from(msg.asInstanceOf[geometry_msgs.PoseWithCovariance])
    else if (rosType == geometry_msgs.Twist._TYPE) from(msg.asInstanceOf[geometry_msgs.Twist])
    else if (rosType == geometry_msgs.TwistStamped._TYPE) from(msg.asInstanceOf[geometry_msgs.TwistStamped])
    else if (rosType == geometry_msgs.TwistWithCovariance._TYPE) from(msg.asInstanceOf[geometry_msgs.TwistWithCovariance])
    else if (rosType == sensor_msgs.Range._TYPE) from(msg.asInstanceOf[sensor_msgs.Range])
    else if (rosType == nav_msgs.Odometry._TYPE) from(msg.asInstanceOf[nav_msgs.Odometry])
    else if (rosType == react_msgs.Mvmt._TYPE) from(msg.asInstanceOf[react_msgs.Mvmt])
    else sys.error("TODO: message type " + rosType + " not yet supported")
  }

  def to(d: Duration): org.ros.message.Duration = new org.ros.message.Duration(d.secs, d.nsecs)
  def to(t: Time): org.ros.message.Time = new org.ros.message.Time(t.secs, t.nsecs)
  
  import org.ros.message._
  import org.ros.node.Node

  def to(node: Node, h: Header): std_msgs.Header = {
    val h2 = node.getTopicMessageFactory().newFromType[std_msgs.Header](h.rosType)
    h2.setSeq(h.seq)
    h2.setStamp(to(h.stamp))
    h2.setFrameId(h.frame_id)
    h2
  }

  def to(node: Node, v: Vector3): geometry_msgs.Vector3 = {
    val v2 = node.getTopicMessageFactory().newFromType[geometry_msgs.Vector3](v.rosType)
    v2.setX(v.x)
    v2.setY(v.y)
    v2.setZ(v.z)
    v2
  }
  
  def to(node: Node, v: Point): geometry_msgs.Point = {
    val v2 = node.getTopicMessageFactory().newFromType[geometry_msgs.Point](v.rosType)
    v2.setX(v.x)
    v2.setY(v.y)
    v2.setZ(v.z)
    v2
  }
  
  def to(node: Node, v: Quaternion): geometry_msgs.Quaternion = {
    val v2 = node.getTopicMessageFactory().newFromType[geometry_msgs.Quaternion](v.rosType)
    v2.setX(v.x)
    v2.setY(v.y)
    v2.setZ(v.z)
    v2.setW(v.w)
    v2
  }

  def to(node: Node, p: Pose2D): geometry_msgs.Pose2D = {
    val p2 = node.getTopicMessageFactory().newFromType[geometry_msgs.Pose2D](p.rosType)
    p2.setX(p.x)
    p2.setY(p.y)
    p2.setTheta(p.theta)
    p2
  }
  
  def to(node: Node, p: Pose): geometry_msgs.Pose = {
    val p2 = node.getTopicMessageFactory().newFromType[geometry_msgs.Pose](p.rosType)
    p2.setPosition(to(node, p.position))
    p2.setOrientation(to(node, p.orientation))
    p2
  }
  
  def to(node: Node, p: PoseWithCovariance): geometry_msgs.PoseWithCovariance = {
    val p2 = node.getTopicMessageFactory().newFromType[geometry_msgs.PoseWithCovariance](p.rosType)
    p2.setPose(to(node, p.pose))
    p2.setCovariance(p.covariance)
    p2
  }
  
  def to(node: Node, t: Twist): geometry_msgs.Twist = {
    val t2 = node.getTopicMessageFactory().newFromType[geometry_msgs.Twist](t.rosType)
    t2.setLinear(to(node, t.linear))
    t2.setAngular(to(node, t.angular))
    t2
  }
  
  def to(node: Node, ts: TwistStamped): geometry_msgs.TwistStamped = {
    val ts2 = node.getTopicMessageFactory().newFromType[geometry_msgs.TwistStamped](ts.rosType)
    ts2.setHeader(to(node, ts.header))
    ts2.setTwist(to(node, ts.twist))
    ts2
  }
  
  def to(node: Node, ts: TwistWithCovariance): geometry_msgs.TwistWithCovariance = {
    val ts2 = node.getTopicMessageFactory().newFromType[geometry_msgs.TwistWithCovariance](ts.rosType)
    ts2.setTwist(to(node, ts.twist))
    ts2.setCovariance(ts.covariance)
    ts2
  }

  def to(node: Node, r: Range): sensor_msgs.Range = {
    val r2 = node.getTopicMessageFactory().newFromType[sensor_msgs.Range](r.rosType)
    r2.setHeader(to(node, r.header))
    r2.setRadiationType(r.kind)
    r2.setMinRange(r.min)
    r2.setMaxRange(r.max)
    r2.setRange(r.range)
    r2
  }
  
  def to(node: Node, r: LaserScan): sensor_msgs.LaserScan = {
    val r2 = node.getTopicMessageFactory().newFromType[sensor_msgs.LaserScan](r.rosType)
    r2.setHeader(to(node, r.header))
    r2.setAngleMin(r.angleMin)
    r2.setAngleMax(r.angleMax)
    r2.setAngleIncrement(r.angleIncrement)
    r2.setTimeIncrement(r.timeIncrement)
    r2.setScanTime(r.scanTime)
    r2.setRangeMin(r.rangeMin)
    r2.setRangeMax(r.rangeMax)
    r2.setRanges(r.ranges)
    r2.setIntensities(r.intensities)
    r2
  }
  
  def to(node: Node, r: Imu): sensor_msgs.Imu = {
    val r2 = node.getTopicMessageFactory().newFromType[sensor_msgs.Imu](r.rosType)
    r2.setHeader(to(node, r.header))
    r2.setOrientation(to(node, r.orientation))
    r2.setOrientationCovariance(r.orientationCovariance)
    r2.setAngularVelocity(to(node, r.angularVelocity))
    r2.setAngularVelocityCovariance(r.angularVelocityCovariance)
    r2.setLinearAcceleration(to(node, r.linearAcceleration))
    r2.setLinearAccelerationCovariance(r.linearAccelerationCovariance)
    r2
  }

  def to(node: Node, o: Odometry): nav_msgs.Odometry = {
    val o2 = node.getTopicMessageFactory().newFromType[nav_msgs.Odometry](o.rosType)
    o2.setHeader(to(node, o.header))
    o2.setChildFrameId(o.childFrameId)
    o2.setPose(to(node, o.pose))
    o2.setTwist(to(node, o.twist))
    o2
  }
  
  def to(node: Node, m: Mvmt): react_msgs.Mvmt = {
    val m2 = node.getTopicMessageFactory().newFromType[react_msgs.Mvmt](m.rosType)
    m2.setHeader(to(node, m.header))
    m2.setSpeed(m.speed)
    m2.setAngularSpeed(m.angular_speed)
    m2.setD(to(m.d))
    m2
  }

  def toMessage(node: Node, m: Message): Any = m match {
    case h: Header => to(node, h)
    case p: Pose2D => to(node, p)
    case p: Pose => to(node, p)
    case p: PoseWithCovariance => to(node, p)
    case v: Point => to(node, v)
    case v: Vector3 => to(node, v)
    case v: Quaternion => to(node, v)
    case t: Twist => to(node, t)
    case ts: TwistStamped => to(node, ts)
    case t: TwistWithCovariance => to(node, t)
    case r: Range => to(node, r)
    case o: Odometry => to(node, o)
    case m: Mvmt => to(node, m)
    case other => sys.error("TODO: message type " + other+ " not fully supported")
  }



}
