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

  def fromHeader(h: std_msgs.Header) = Header(h.getSeq, fromTime(h.getStamp), h.getFrameId)

  def fromPose2D(p: geometry_msgs.Pose2D) = Pose2D(p.getX, p.getY, p.getTheta)

  def fromMvmt(m: react_msgs.Mvmt) = Mvmt(fromHeader(m.getHeader), m.getSpeed, m.getAngularSpeed, fromDuration(m.getD))


}

class MessageConverter(node: org.ros.node.Node) {


  def toDuration(d: Duration) = new org.ros.message.Duration(d.secs, d.nsecs)
  
  def toTime(t: Time) = new org.ros.message.Time(t.secs, t.nsecs)
  
  import org.ros.message._

  def toHeader(h: Header) = {
    val h2 = node.getTopicMessageFactory().newFromType[std_msgs.Header](std_msgs.Header._TYPE)
    h2.setSeq(h.seq)
    h2.setStamp(toTime(h.stamp))
    h2.setFrameId(h.frame_id)
    h2
  }

  def toPose2D(p: Pose2D) = {
    val p2 = node.getTopicMessageFactory().newFromType[geometry_msgs.Pose2D](geometry_msgs.Pose2D._TYPE)
    p2.setX(p.x)
    p2.setY(p.y)
    p2.setTheta(p.theta)
    p2
  }

  def toMvmt(m: Mvmt) = {
    val m2 = node.getTopicMessageFactory().newFromType[react_msgs.Mvmt](react_msgs.Mvmt._TYPE)
    m2.setHeader(toHeader(m.header))
    m2.setSpeed(m.speed)
    m2.setAngularSpeed(m.angular_speed)
    m2.setD(toDuration(m.d))
    m2
  }

}
