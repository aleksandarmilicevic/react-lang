package react.verification.modelchecker

import react.verification._
import java.nio.ByteBuffer
import react.runtime.ScheduledTask
import scala.collection.mutable.PriorityQueue

import ModelChecker._


class SchedulingPoint(tasks: List[ScheduledTask], scheduler: Scheduler, exec: WorldExecutor) extends BranchingPoint {

  //insert a filter function before toList in perms

  val expiration = tasks.head.expires
  assert(tasks.forall(_.expires == expiration))

  private val com = new CommutativityAnalysis(exec, tasks)
  private def keep(lst : List[ScheduledTask]): Boolean = {
    lst.sliding(2).forall( l => {
      l.length != 2 ||
      !com.commutes(l(0), l(1)) ||
      l(0).hashCode <= l(1).hashCode
    })
  }

  val perms = tasks.permutations.filter(keep).toList
  //val perms = tasks.permutations.toList

  def alternatives = perms.length

  def act(alt: Int) = {
    for (t <- perms(alt)) yield {
      t.expires = expiration //reset the expirations since the tasks are not in the scheduler.
      t.fct()
      if (t.isPeriodic && !t.cancelled) {
        scheduler.schedule(t)
      }
      t.descr
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

  def nextBP(exec: WorldExecutor): SchedulingPoint = new SchedulingPoint(nextTasks, this, exec)

  //all the tasks that expire at the same time
  def nextTasks: List[ScheduledTask] = {
    removeCanceled //this cannot hurt
    if (queue.isEmpty) {
      Nil
    } else {
      //println(toString)
      val old = queue.size
      val task = queue.dequeue
      assert(queue.size + 1 == old)
      var same = List[ScheduledTask](task)
      while(!queue.isEmpty && queue.head.expires == task.expires) {
        same = queue.dequeue :: same
      }
      //println(toString)
      //println(same)
      assert(old == queue.size + same.size, "nextTasks: queue sizes do not agree (" + old + " ≠ " + (queue.size + same.size) +")")
      normalize(same)
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
      val ts = nextTasks
      val idx = ts.map(cache.idx(_)).sorted
      for (i <- idx) {
        if (bytePerTask == 1)      buffer.put(i.toByte)
        else if (bytePerTask == 2) buffer.putShort(i.toShort)
        else if (bytePerTask == 4) buffer.putInt(i)
        else sys.error("bytePerTask has an incorrect size")
      }
      content = ts ::: content
    }
    for (t <- content) queue.enqueue(t)
    assert(size == 2 + queue.size* bytePerTask, "error while sazing scheduler state")
    buffer.array
  }

  def compactState = {
    removeCanceled
    val ts = content.map(cache.idx)
    val idx = Scheduler.scheduleIdx(ts)
    val buffer = ByteBuffer.allocate(2)
    buffer.putShort(idx)
    buffer.array
  }
  
  def restoreTasks(now: Long, lst: Iterable[ScheduledTask]) {
    queue.clear
    _now = now
    var t = _now + 1 //assumption that we don't have task scheduled now
    for (task <- lst) {
      //find the next expiration
      val exp = math.ceil(t/task.period.toDouble).toInt * task.period
      task.expires = exp
      t = exp
      task.cancelled = false
      queue.enqueue(task)
    }
  }

  def restoreState(in: State) {
    val buffer = ByteBuffer.wrap(in)
    queue.clear
    val now = buffer.getShort
    val tasks = buffer.remaining / bytePerTask
    var t = _now + 1 //assumption that we don't have task scheduled now
    val ts = for (i <- 0 until tasks) yield {
      //restore the task (and their expiration)
      val index: Int =
        if (bytePerTask == 1)      buffer.get
        else if (bytePerTask == 2) buffer.getShort
        else if (bytePerTask == 4) buffer.getInt
        else sys.error("bytePerTask has an incorrect size")
      cache.value(index)
    }
    restoreTasks(now, ts)
  }
  
  def restoreCompact(in: State) {
    val buffer = ByteBuffer.wrap(in)
    val ts = Scheduler.scheduleVal(buffer.getShort)
    val tts = ts.map(cache.value)
    restoreTasks(0, tts)
  }

  def content = normalize(queue.toList)

  val cache = new Cache[ScheduledTask]
  
  def warmUpCache(task: ScheduledTask) {
    cache.idx(task)
  }


  protected def normalize(sch: List[ScheduledTask]) = {
    def compare(a: ScheduledTask, b: ScheduledTask) = {
      if  (a.period == b.period) {
        cache.idx(a) < cache.idx(b)
      } else {
        a.period < b.period
      }
    }
    sch.sortWith(compare)
  }

  def shift(t: Long) = {
    _now = _now - t
    //TODO should we reenqueue the elements ?
    for (task <- queue) {
      task.expires -= t
    }
  }

}

object Scheduler {

  /** cach for whole schedule */
  private val cache = new Cache[List[Int]]

  def scheduleIdx(sch: List[Int]): Short = {
    cache.idx(sch).toShort
  }

  def scheduleVal(idx: Short): List[Int] = { 
    cache.value(idx)
  }

  /** return the LCM of the period of all task in the queue (empty queue has period 1) */
  def computePeriod(tasks: Iterable[ScheduledTask]) = {
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
    tasks.foldLeft(1)( (acc, t) => if (t.period > 0) lcm(acc, t.period) else acc )
  }


}
