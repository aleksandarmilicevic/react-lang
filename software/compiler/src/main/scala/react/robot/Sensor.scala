package react.robot

import react._
import react.message._

//some utils methods for sensors

//returns the closest object distance
object GetRange {
  def unapply(m: Message): Option[Float] = m match {
    case r: Range => Some(r.range)
    case l: LaserScan => Some(l.ranges.min)
    case _ => None
  }
}
