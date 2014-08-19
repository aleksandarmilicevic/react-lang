package react.runtime

import scala.collection.mutable.PriorityQueue
import scala.math.Ordering.Implicits

class ScheduledTask(val period: Int,
                    val fct: () => Unit,
                    var expires: Long = -1,
                    var cancelled: Boolean = false
                   ) extends java.lang.Comparable[ScheduledTask] {

  def compareTo(other: ScheduledTask) = other.expires.compareTo(expires)

  def isPeriodic = period > 0

  def cancel { cancelled = true }

  def waitExpiration = {
    val t = expires - java.lang.System.currentTimeMillis()
    if (t > 1) Thread.sleep(t)
  }

}


class Scheduler {

  private val queue = new PriorityQueue[ScheduledTask]()

  private def now = java.lang.System.currentTimeMillis()

  def addSingleTask(delay: Int, fct: () => Unit) {
    val task = new ScheduledTask(-1, fct, now + delay)
    queue.enqueue(task)
  }

  def waitUntilNextTask: Option[ScheduledTask] = {
    if (queue.isEmpty) {
      None
    } else {
      val task = queue.dequeue
      if (task.cancelled) {
        waitUntilNextTask
      } else {
        task.waitExpiration
        Some(task)
      }
    }
  }
  
  def schedule(t: ScheduledTask) = {
    if (t.expires == -1 || t.cancelled) {
      assert(t.period > 0, "period must be ≥ 1")
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
