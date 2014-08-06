package react.message

//TODO wrapper around the REACT-ROS messages

abstract class Message { }

case class Time(secs: Int, nsecs: Int) extends Message

case class Duration(secs: Int, nsecs: Int) extends Message

case class Header(seq: Int, stamp: Time, frame_id: String) extends Message

case class Pose2D(x: Double, y: Double, theta: Double) extends Message

case class Mvmt(header: Header, speed: Double, angular_speed: Double, d: Duration) extends Message

object Message {

  def time(ms: Long) = Time((ms/1000).toInt, ((ms % 1000) * 1000).toInt)
  def duration(ms: Long) = Duration((ms/1000).toInt, ((ms % 1000) * 1000).toInt)

  def fromDuration(d: org.ros.message.Duration) = Duration(d.secs, d.nsecs)

  def fromTime(t: org.ros.message.Time) = Time(t.secs, t.nsecs)

  def fromHeader(h: org.ros.rosjava_messages.std_msgs.Header) = Header(h.seq, fromTime(h.stamp), h.frame_id)

  def fromPose2D(p: org.ros.rosjava_messages.geometry_msgs.Pose2D) = Pose2D(p.x, p.y, p.theta)

  def fromMvmt(m: react_msgs.Mvmt) = Mvmt(fromHeader(m.getHeader), m.speed, m.angular_speed, fromDuration(m.d))


  def toDuration(d: Duration) = new org.ros.message.Duration(d.secs, d.nsecs)
  
  def toTime(t: Time) = new org.ros.message.Time(t.secs, t.nsecs)

  def toHeader(h: Header) = new org.ros.message.std_msgs.Header(h.seq, toTime(h.stamp), h.frame_id)

  def toPose2D(p: Pose2D) = new org.ros.message.geometry_msgs.Pose2D(p.x, p.y, p.theta)

  def toMvmt(m: Mvmt) = new org.ros.rosjava_messages.Mvmt(toHeader(m.header), m.speed, m.angular_speed, toDuration(m.d))

}
