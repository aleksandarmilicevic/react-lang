package react.verification

import java.nio.ByteBuffer
import react.runtime.ScheduledTask
import scala.collection.mutable.PriorityQueue

class SchedulingPoint(tasks: List[ScheduledTask], scheduler: Scheduler) extends BranchingPoint {

  val perms = tasks.permutations.toList

  def alternatives = perms.length

  def act(alt: Int) {
    for (t <- perms(alt)) {
      t.fct()
      if (t.isPeriodic) {
        scheduler.schedule(t)
      }
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

  def elapse(t: Long) {
    _now += t
  }

  def nextBP: SchedulingPoint = new SchedulingPoint(nextTasks, this)

  //all the tasks that expire at the same time
  def nextTasks: List[ScheduledTask] = {
    removeCanceled //this cannot hurt
    if (queue.isEmpty) {
      Nil
    } else {
      val old = queue.size
      val task = queue.dequeue
      val same = queue.takeWhile(_.expires == task.expires).toList
      while(!queue.isEmpty && queue.head.expires == task.expires) {
        queue.dequeue
      }
      assert(old == queue.size + 1 + same.size, "nextTasks: queue size does not agree")
      task :: same
    }
  }

  override def toString = {
    val buffer = new StringBuilder(1024)
    buffer.append("Scheduler {\n")
    buffer.append("  now = " + now + "\n")
    buffer.append("  tasks:\n")
    for (t <- queue) {
      buffer.append("    ")
      buffer.append(t.toString)
      buffer.append("\n")
    }
    buffer.append("}\n")
    buffer.toString
  }


  val bytePerTask = 1

  def saveState = {
    // 2: current time
    // for the expiration, we assume that all task are periodic and start at time 0
    removeCanceled
    val size = 2 + queue.size* bytePerTask
    val buffer = ByteBuffer.allocate(size)
    buffer.putShort(now.toShort)
    var content = List[ScheduledTask]()
    while (!queue.isEmpty) {
      val t = queue.dequeue
      val idx = cache.idx(t)
      if (bytePerTask == 1)      buffer.put(idx.toByte)
      else if (bytePerTask == 2) buffer.putShort(idx.toShort)
      else if (bytePerTask == 4) buffer.putInt(idx)
      else sys.error("bytePerTask has an incorrect size")
      content = t :: content
    }
    for (t <- content) queue.enqueue(t)
    assert(size == 2 + queue.size* bytePerTask, "error while sazing scheduler state")
    buffer.array
  }

  def restoreState(in: Array[Byte]) {
    val buffer = ByteBuffer.wrap(in)
    queue.clear
    _now = buffer.getShort
    val tasks = buffer.remaining / bytePerTask
    var t = _now + 1 //assumption that we don't have task scheduled now
    for (i <- 0 until tasks){
      //restore the task (and their expiration)
      val index: Int =
        if (bytePerTask == 1)      buffer.get
        else if (bytePerTask == 2) buffer.getShort
        else if (bytePerTask == 4) buffer.getInt
        else sys.error("bytePerTask has an incorrect size")
      val task = cache.value(index)
      //find the next expiration
      val exp = math.ceil(t/task.period.toDouble).toInt * task.period
      task.expires = exp
      t = exp
      task.cancelled = false
      queue.enqueue(task)
    }
  }

  val cache = new Cache[ScheduledTask](bytePerTask)

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

  def shift(t: Long) = {
    _now = _now - t
    //TODO should we reenqueue the elements ?
    for (task <- queue) {
      task.expires -= t
    }
  }

}
