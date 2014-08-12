package react.runtime

import scala.collection.mutable.PriorityQueue
import scala.math.Ordering.Implicits

class ScheduledTask(var expires: Long,
                    val period: Int,
                    val fct: () => Unit
                   ) extends java.lang.Comparable[ScheduledTask] {
  def compareTo(other: ScheduledTask) = other.expires.compareTo(expires)
}


class Scheduler {

  private val queue = new PriorityQueue[ScheduledTask]()

  private def now = java.lang.System.currentTimeMillis()

  def addTask(period: Int, fct: () => Unit) {
    assert(period >= 1, "period needs to be at least 1ms")
    val task = new ScheduledTask(now + period, period, fct)
    queue.enqueue(task)
  }

  def waitUntilNextTask: Option[ScheduledTask] = {
    if (queue.isEmpty) {
      None
    } else {
      val task = queue.dequeue
      val wait = task.expires - now
      Thread.sleep(wait)
      Some(task)
    }
  }

  def reschedule(t: ScheduledTask) = {
    t.expires += t.period
    queue.enqueue(t)
  }

}
