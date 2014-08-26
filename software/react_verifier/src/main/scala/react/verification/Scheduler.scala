package react.verification

import react.runtime.ScheduledTask
import scala.collection.mutable.PriorityQueue

class SchedulingPoint(tasks: List[ScheduledTask], scheduler: Scheduler) extends BranchingPoint {

  def alternatives = tasks.length

  def act(alt: Int) {
    val elt = tasks(alt)
    elt.fct()
    if (elt.isPeriodic) {
      scheduler.schedule(elt)
    }
  }

}


class Scheduler extends react.runtime.Scheduler {

  var _now = 0l
  override def now = _now
  
  def timeToNext = {
    val next = queue.headOption.map(_.expires).getOrElse(now)
    next - now
  }

  def nextBP: SchedulingPoint = new SchedulingPoint(nextTasks, this)

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
    _now = _now - t
    //TODO should we reenqueue the elements ?
    for (task <- queue) {
      task.expires -= t
    }
  }

}
