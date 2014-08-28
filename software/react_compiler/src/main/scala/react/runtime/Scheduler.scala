package react.runtime

import scala.collection.mutable.PriorityQueue
import scala.math.Ordering.Implicits

class ScheduledTask(val period: Int,
                    val fct: () => Unit,
                    var expires: Long = -1,
                    var cancelled: Boolean = false
                   ) extends java.lang.Comparable[ScheduledTask] {

  override def toString = "ScheduledTask(period = "+period+", expires = "+expires+", cancelled = "+cancelled+", " + fct + ")"

  def compareTo(other: ScheduledTask) = other.expires.compareTo(expires)

  def isPeriodic = period > 0

  def cancel { cancelled = true }

  def waitExpiration = {
    val t = expires - java.lang.System.currentTimeMillis()
    if (t > 1) Thread.sleep(t)
  }

}


class Scheduler {

  protected val queue = new PriorityQueue[ScheduledTask]()

  protected def now = java.lang.System.currentTimeMillis()

  def addSingleTask(delay: Int, fct: () => Unit) {
    val task = new ScheduledTask(-1, fct, now + delay)
    queue.enqueue(task)
  }
  
  def nextTask: Option[ScheduledTask] = {
    if (queue.isEmpty) {
      None
    } else {
      val task = queue.dequeue
      if (task.cancelled) {
        nextTask
      } else {
        Some(task)
      }
    }
  }

  def waitUntilNextTask: Option[ScheduledTask] = {
    val nt = nextTask
    for (t <- nt) t.waitExpiration
    nt
  }
  
  def schedule(t: ScheduledTask) = {
    assert(t.period > 0, "period must be ≥ 1")
    if (t.expires == -1 || t.cancelled) {
      t.expires = now + t.period
      t.cancelled = false
    } else {
      //TODO a warning is if the new expiration is less than now
      t.expires += t.period
    }
    queue.enqueue(t)
  }

  def removeCanceled = {
    queue.filterNot(_.cancelled)
  }

}
