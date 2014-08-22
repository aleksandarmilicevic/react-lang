package react.verification

import react.runtime.ScheduledTask
import scala.collection.mutable.PriorityQueue

class Scheduler {

  private val queue = new PriorityQueue[ScheduledTask]()

  var now = 0l
  
  def timeToNext = {
    val next = queue.headOption.map(_.expires).getOrElse(now)
    next - now
  }

  //all the tasks that expire at the same time
  def nextTasks: List[ScheduledTask] = {
    removeCanceled //this cannot hurt
    if (queue.isEmpty) {
      Nil
    } else {
      val task = queue.dequeue
      val same = queue.takeWhile(_.expires == task.expires)
      queue.dropWhile(_.expires == task.expires) //take while does not modify the original queue
      task :: same.toList
    }
  }

  def addSingleTask(delay: Int, fct: () => Unit) {
    val task = new ScheduledTask(-1, fct, now + delay)
    queue.enqueue(task)
  }
  
  def schedule(t: ScheduledTask) = {
    assert(t.period > 0, "period must be ≥ 1")
    if (t.expires == -1 || t.cancelled) {
      t.expires = now + t.period
      t.cancelled = false
    } else {
      t.expires += t.period
    }
    queue.enqueue(t)
  }

  def removeCanceled = {
    queue.filterNot(_.cancelled)
  }

  /** return the LCM of the period of all task in the queue (empty queue has period 1) */
  def computePeriod = {
    // http://algorithmguru.com/content/?viewpage=./contentfiles/showalgo.php&id=17&type=r
    def lcm(m: Int, n: Int) = {
      var a = m
      var b = n
      while (a != b) {
        if (a < b) a += m
        else       b += n
      }
      a
    }
    queue.foldLeft(1)( (acc, t) => if (t.period > 0) lcm(acc, t.period) else acc )
  }

  def shift(t: Int) = {
    now = now - t
    for (task <- queue) {
      task.expires -= t
    }
  }

}
