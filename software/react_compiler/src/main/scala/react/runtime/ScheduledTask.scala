package react.runtime

import scala.math.Ordering.Implicits

class ScheduledTask(val descr: String,
                    val period: Int,
                    val fct: () => Unit,
                    var expires: Long = -1,
                    var cancelled: Boolean = false,
                    val publish: Option[List[(String,String)]] = None
                   ) extends java.lang.Comparable[ScheduledTask] {

  override def toString = "ScheduledTask["+descr+"](period = "+period+", expires = "+expires+", cancelled = "+cancelled+")"

  def compareTo(other: ScheduledTask) = other.expires.compareTo(expires)

  def isPeriodic = period > 0

  def cancel { cancelled = true }

  def waitExpiration = {
    val t = expires - java.lang.System.currentTimeMillis()
    if (t > 1) Thread.sleep(t)
  }

}

