package react.message

/* TODO automatically generate all (most of) this (using Macros)
 * adding new messages also requires fiiling some type info in react.rewriting.Handlers
 */

case class Time(secs: Int, nsecs: Int) {
  def toMillis = secs * 1000 + nsecs / 1000000
}
case class Duration(secs: Int, nsecs: Int) {
  def toMillis = secs * 1000 + nsecs / 1000000
}

abstract class Message(val rosType: String) { }

//std_msgs
case class Header(seq: Int, stamp: Time, frame: String) extends Message(std_msgs.Header._TYPE)

//react_msgs
case class Mvmt(header: Header, speed: Double, angular_speed: Double, d: Duration) extends Message(react_msgs.Mvmt._TYPE)

//gazebo_msgs
case class ModelState(
  model_name: String,
  pose: Pose, 
  twist: Twist, 
  reference_frame: String) extends Message(gazebo_msgs.ModelState._TYPE)


//TODO refactor this to make it more modular ...
object Message {

  def time(ms: Long) = Time((ms/1000).toInt, ((ms % 1000000) * 1000000).toInt)
  def duration(ms: Long) = Duration((ms/1000).toInt, ((ms % 1000000) * 1000000).toInt)

  def from(d: org.ros.message.Duration): Duration = Duration(d.secs, d.nsecs)
  def from(t: org.ros.message.Time): Time = Time(t.secs, t.nsecs)

  def from(h: std_msgs.Header): Header = Header(h.getSeq, from(h.getStamp), h.getFrameId)

  def from(v: geometry_msgs.Point): Point = Point(v.getX, v.getY, v.getZ)
  def from(v: geometry_msgs.Vector3): Vector3 = Vector3(v.getX, v.getY, v.getZ)
  def from(v: geometry_msgs.Quaternion): Quaternion = Quaternion(v.getX, v.getY, v.getZ, v.getW)
  def from(p: geometry_msgs.Pose2D): Pose2D = Pose2D(p.getX, p.getY, p.getTheta)
  def from(p: geometry_msgs.Pose): Pose = Pose(from(p.getPosition), from(p.getOrientation))
  def from(p: geometry_msgs.PoseStamped): PoseStamped = PoseStamped(from(p.getHeader), from(p.getPose))
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

  def from(m: gazebo_msgs.ModelState): ModelState = {
    ModelState(m.getModelName(),
      from(m.getPose()),
      from(m.getTwist()),
      m.getReferenceFrame())
  }

  def from(o: nav_msgs.Path): Path = {
    val header = from(o.getHeader)
    val poses = o.getPoses.toArray[geometry_msgs.PoseStamped](Array[geometry_msgs.PoseStamped]()).map(from)
    Path(header, poses)
  }

  def from(m: react_msgs.Mvmt): Mvmt = Mvmt(from(m.getHeader), m.getSpeed, m.getAngularSpeed, from(m.getD))
  
  def from(s: std_msgs.Empty): Primitive.Empty.type = Primitive.Empty
  def from(s: std_msgs.Bool): Primitive.Bool = Primitive.Bool(s.getData)
  def from(s: std_msgs.Byte): Primitive.Byte = Primitive.Byte(s.getData)
  def from(s: std_msgs.Char): Primitive.Char = Primitive.Char(s.getData.asInstanceOf[Char])
  def from(s: std_msgs.Int16): Primitive.Int16 = Primitive.Int16(s.getData)
  def from(s: std_msgs.Int32): Primitive.Int32 = Primitive.Int32(s.getData)
  def from(s: std_msgs.Int64): Primitive.Int64 = Primitive.Int64(s.getData)
  def from(s: std_msgs.Float32): Primitive.Float32 = Primitive.Float32(s.getData)
  def from(s: std_msgs.Float64): Primitive.Float64 = Primitive.Float64(s.getData)
  def from(s: std_msgs.String): Primitive.String = Primitive.String(s.getData)
  def from(s: std_msgs.Duration): Primitive.Duration = Primitive.Duration(from(s.getData).toMillis)
  def from(s: std_msgs.Time): Primitive.Time = Primitive.Time(from(s.getData).toMillis)

  def fromMessage(rosType: String, msg: Any): Message = {
    if (rosType == std_msgs.Header._TYPE)                        from(msg.asInstanceOf[std_msgs.Header])
    else if (rosType == geometry_msgs.Point._TYPE)               from(msg.asInstanceOf[geometry_msgs.Point])
    else if (rosType == geometry_msgs.Vector3._TYPE)             from(msg.asInstanceOf[geometry_msgs.Vector3])
    else if (rosType == geometry_msgs.Quaternion._TYPE)          from(msg.asInstanceOf[geometry_msgs.Quaternion])
    else if (rosType == geometry_msgs.Pose2D._TYPE)              from(msg.asInstanceOf[geometry_msgs.Pose2D])
    else if (rosType == geometry_msgs.Pose._TYPE)                from(msg.asInstanceOf[geometry_msgs.Pose])
    else if (rosType == geometry_msgs.PoseStamped._TYPE)         from(msg.asInstanceOf[geometry_msgs.PoseStamped])
    else if (rosType == geometry_msgs.PoseWithCovariance._TYPE)  from(msg.asInstanceOf[geometry_msgs.PoseWithCovariance])
    else if (rosType == geometry_msgs.Twist._TYPE)               from(msg.asInstanceOf[geometry_msgs.Twist])
    else if (rosType == geometry_msgs.TwistStamped._TYPE)        from(msg.asInstanceOf[geometry_msgs.TwistStamped])
    else if (rosType == geometry_msgs.TwistWithCovariance._TYPE) from(msg.asInstanceOf[geometry_msgs.TwistWithCovariance])
    else if (rosType == sensor_msgs.Range._TYPE)                 from(msg.asInstanceOf[sensor_msgs.Range])
    else if (rosType == sensor_msgs.Imu._TYPE)                   from(msg.asInstanceOf[sensor_msgs.Imu])
    else if (rosType == sensor_msgs.LaserScan._TYPE)             from(msg.asInstanceOf[sensor_msgs.LaserScan])
    else if (rosType == nav_msgs.Odometry._TYPE)                 from(msg.asInstanceOf[nav_msgs.Odometry])
    else if (rosType == nav_msgs.Path._TYPE)                     from(msg.asInstanceOf[nav_msgs.Path])
    else if (rosType == react_msgs.Mvmt._TYPE)                   from(msg.asInstanceOf[react_msgs.Mvmt])
    else if (rosType == gazebo_msgs.ModelState._TYPE)            from(msg.asInstanceOf[gazebo_msgs.ModelState])
    else if (rosType == std_msgs.Empty._TYPE)                    from(msg.asInstanceOf[std_msgs.Empty])
    else if (rosType == std_msgs.Bool._TYPE)                     from(msg.asInstanceOf[std_msgs.Bool])
    else if (rosType == std_msgs.Byte._TYPE)                     from(msg.asInstanceOf[std_msgs.Byte])
    else if (rosType == std_msgs.Char._TYPE)                     from(msg.asInstanceOf[std_msgs.Char])
    else if (rosType == std_msgs.Int16._TYPE)                    from(msg.asInstanceOf[std_msgs.Int16])
    else if (rosType == std_msgs.Int32._TYPE)                    from(msg.asInstanceOf[std_msgs.Int32])
    else if (rosType == std_msgs.Int64._TYPE)                    from(msg.asInstanceOf[std_msgs.Int64])
    else if (rosType == std_msgs.Float32._TYPE)                  from(msg.asInstanceOf[std_msgs.Float32])
    else if (rosType == std_msgs.Float64._TYPE)                  from(msg.asInstanceOf[std_msgs.Float64])
    else if (rosType == std_msgs.String._TYPE)                   from(msg.asInstanceOf[std_msgs.String])
    else if (rosType == std_msgs.Duration._TYPE)                 from(msg.asInstanceOf[std_msgs.Duration])
    else if (rosType == std_msgs.Time._TYPE)                     from(msg.asInstanceOf[std_msgs.Time])
    else sys.error("TODO: message type " + rosType + " not yet supported")
  }

  def to(d: Duration): org.ros.message.Duration = new org.ros.message.Duration(d.secs, d.nsecs)
  def to(t: Time): org.ros.message.Time = new org.ros.message.Time(t.secs, t.nsecs)
  
  import org.ros.message._
  import org.ros.node.Node

  def to(node: MessageFactory, h: Header): std_msgs.Header = {
    val h2 = node.newFromType[std_msgs.Header](h.rosType)
    h2.setSeq(h.seq)
    h2.setStamp(to(h.stamp))
    h2.setFrameId(h.frame)
    h2
  }

  def to(node: MessageFactory, v: Vector3): geometry_msgs.Vector3 = {
    val v2 = node.newFromType[geometry_msgs.Vector3](v.rosType)
    v2.setX(v.x)
    v2.setY(v.y)
    v2.setZ(v.z)
    v2
  }
  
  def to(node: MessageFactory, v: Point): geometry_msgs.Point = {
    val v2 = node.newFromType[geometry_msgs.Point](v.rosType)
    v2.setX(v.x)
    v2.setY(v.y)
    v2.setZ(v.z)
    v2
  }
  
  def to(node: MessageFactory, v: Quaternion): geometry_msgs.Quaternion = {
    val v2 = node.newFromType[geometry_msgs.Quaternion](v.rosType)
    v2.setX(v.x)
    v2.setY(v.y)
    v2.setZ(v.z)
    v2.setW(v.w)
    v2
  }

  def to(node: MessageFactory, p: Pose2D): geometry_msgs.Pose2D = {
    val p2 = node.newFromType[geometry_msgs.Pose2D](p.rosType)
    p2.setX(p.x)
    p2.setY(p.y)
    p2.setTheta(p.theta)
    p2
  }
  
  def to(node: MessageFactory, p: Pose): geometry_msgs.Pose = {
    val p2 = node.newFromType[geometry_msgs.Pose](p.rosType)
    p2.setPosition(to(node, p.position))
    p2.setOrientation(to(node, p.orientation))
    p2
  }
  
  def to(node: MessageFactory, p: PoseStamped): geometry_msgs.PoseStamped = {
    val p2 = node.newFromType[geometry_msgs.PoseStamped](p.rosType)
    p2.setHeader(to(node, p.header))
    p2.setPose(to(node, p.pose))
    p2
  }
  
  def to(node: MessageFactory, p: PoseWithCovariance): geometry_msgs.PoseWithCovariance = {
    val p2 = node.newFromType[geometry_msgs.PoseWithCovariance](p.rosType)
    p2.setPose(to(node, p.pose))
    p2.setCovariance(p.covariance)
    p2
  }
  
  def to(node: MessageFactory, t: Twist): geometry_msgs.Twist = {
    val t2 = node.newFromType[geometry_msgs.Twist](t.rosType)
    t2.setLinear(to(node, t.linear))
    t2.setAngular(to(node, t.angular))
    t2
  }
  
  def to(node: MessageFactory, ts: TwistStamped): geometry_msgs.TwistStamped = {
    val ts2 = node.newFromType[geometry_msgs.TwistStamped](ts.rosType)
    ts2.setHeader(to(node, ts.header))
    ts2.setTwist(to(node, ts.twist))
    ts2
  }
  
  def to(node: MessageFactory, ts: TwistWithCovariance): geometry_msgs.TwistWithCovariance = {
    val ts2 = node.newFromType[geometry_msgs.TwistWithCovariance](ts.rosType)
    ts2.setTwist(to(node, ts.twist))
    ts2.setCovariance(ts.covariance)
    ts2
  }

  def to(node: MessageFactory, r: Range): sensor_msgs.Range = {
    val r2 = node.newFromType[sensor_msgs.Range](r.rosType)
    r2.setHeader(to(node, r.header))
    r2.setRadiationType(r.kind)
    r2.setMinRange(r.min)
    r2.setMaxRange(r.max)
    r2.setRange(r.range)
    r2
  }
  
  def to(node: MessageFactory, r: LaserScan): sensor_msgs.LaserScan = {
    val r2 = node.newFromType[sensor_msgs.LaserScan](r.rosType)
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
  
  def to(node: MessageFactory, r: Imu): sensor_msgs.Imu = {
    val r2 = node.newFromType[sensor_msgs.Imu](r.rosType)
    r2.setHeader(to(node, r.header))
    r2.setOrientation(to(node, r.orientation))
    r2.setOrientationCovariance(r.orientationCovariance)
    r2.setAngularVelocity(to(node, r.angularVelocity))
    r2.setAngularVelocityCovariance(r.angularVelocityCovariance)
    r2.setLinearAcceleration(to(node, r.linearAcceleration))
    r2.setLinearAccelerationCovariance(r.linearAccelerationCovariance)
    r2
  }

  def to(node: MessageFactory, o: Odometry): nav_msgs.Odometry = {
    val o2 = node.newFromType[nav_msgs.Odometry](o.rosType)
    o2.setHeader(to(node, o.header))
    o2.setChildFrameId(o.childFrameId)
    o2.setPose(to(node, o.pose))
    o2.setTwist(to(node, o.twist))
    o2
  }
  
  def to(node: MessageFactory, o: Path): nav_msgs.Path = {
    val o2 = node.newFromType[nav_msgs.Path](o.rosType)
    o2.setHeader(to(node, o.header))
    val array: Array[geometry_msgs.PoseStamped] = o.poses.map(to(node, _))
    o2.setPoses(java.util.Arrays.asList[geometry_msgs.PoseStamped](array: _*))
    o2
  }
  
  def to(node: MessageFactory, m: Mvmt): react_msgs.Mvmt = {
    val m2 = node.newFromType[react_msgs.Mvmt](m.rosType)
    m2.setHeader(to(node, m.header))
    m2.setSpeed(m.speed)
    m2.setAngularSpeed(m.angular_speed)
    m2.setD(to(m.d))
    m2
  }

  def to(node: MessageFactory, m: ModelState): gazebo_msgs.ModelState = {
    val m2 = node.newFromType[gazebo_msgs.ModelState](m.rosType)
    m2.setModelName(m.model_name)
    m2.setPose(to(node, m.pose))
    m2.setTwist(to(node, m.twist))
    m2.setReferenceFrame(m.reference_frame)
    m2
  }
  
  def to(node: MessageFactory, p: Primitive.Empty.type): std_msgs.Empty = {
    val p2 = node.newFromType[std_msgs.Empty](p.rosType)
    p2
  }
  
  def to(node: MessageFactory, p: Primitive.Bool): std_msgs.Bool = {
    val p2 = node.newFromType[std_msgs.Bool](p.rosType)
    p2.setData(p.data)
    p2
  }

  def to(node: MessageFactory, p: Primitive.Byte): std_msgs.Byte = {
    val p2 = node.newFromType[std_msgs.Byte](p.rosType)
    p2.setData(p.data)
    p2
  }

  def to(node: MessageFactory, p: Primitive.Char): std_msgs.Char = {
    val p2 = node.newFromType[std_msgs.Char](p.rosType)
    p2.setData(p.data.toByte)
    p2
  }

  def to(node: MessageFactory, p: Primitive.Int16): std_msgs.Int16 = {
    val p2 = node.newFromType[std_msgs.Int16](p.rosType)
    p2.setData(p.data)
    p2
  }

  def to(node: MessageFactory, p: Primitive.Int32): std_msgs.Int32 = {
    val p2 = node.newFromType[std_msgs.Int32](p.rosType)
    p2.setData(p.data)
    p2
  }

  def to(node: MessageFactory, p: Primitive.Int64): std_msgs.Int64 = {
    val p2 = node.newFromType[std_msgs.Int64](p.rosType)
    p2.setData(p.data)
    p2
  }

  def to(node: MessageFactory, p: Primitive.Float32): std_msgs.Float32 = {
    val p2 = node.newFromType[std_msgs.Float32](p.rosType)
    p2.setData(p.data)
    p2
  }

  def to(node: MessageFactory, p: Primitive.Float64): std_msgs.Float64 = {
    val p2 = node.newFromType[std_msgs.Float64](p.rosType)
    p2.setData(p.data)
    p2
  }

  def to(node: MessageFactory, p: Primitive.String): std_msgs.String = {
    val p2 = node.newFromType[std_msgs.String](p.rosType)
    p2.setData(p.data)
    p2
  }

  def to(node: MessageFactory, p: Primitive.Duration): std_msgs.Duration = {
    val p2 = node.newFromType[std_msgs.Duration](p.rosType)
    p2.setData(to(duration(p.data)))
    p2
  }

  def to(node: MessageFactory, p: Primitive.Time): std_msgs.Time = {
    val p2 = node.newFromType[std_msgs.Time](p.rosType)
    p2.setData(to(time(p.data)))
    p2
  }

  def toMessage(node: MessageFactory, m: Message): Any = m match {
    case h: Header              => to(node, h)
    case p: Pose2D              => to(node, p)
    case p: Pose                => to(node, p)
    case p: PoseWithCovariance  => to(node, p)
    case v: Point               => to(node, v)
    case v: Vector3             => to(node, v)
    case v: Quaternion          => to(node, v)
    case t: Twist               => to(node, t)
    case ts: TwistStamped       => to(node, ts)
    case t: TwistWithCovariance => to(node, t)
    case r: Range               => to(node, r)
    case i: Imu                 => to(node, i)
    case l: LaserScan           => to(node, l)
    case o: Odometry            => to(node, o)
    case m: Mvmt                => to(node, m)
    case m: ModelState          => to(node, m)
    case Primitive.Empty        => to(node, Primitive.Empty)
    case m: Primitive.Byte      => to(node, m)
    case m: Primitive.Bool      => to(node, m)
    case m: Primitive.Char      => to(node, m)
    case m: Primitive.Int16     => to(node, m)
    case m: Primitive.Int32     => to(node, m)
    case m: Primitive.Int64     => to(node, m)
    case m: Primitive.String    => to(node, m)
    case m: Primitive.Duration  => to(node, m)
    case m: Primitive.Time      => to(node, m)
    case other => sys.error("TODO: message type " + other+ " not fully supported")
  }



}
