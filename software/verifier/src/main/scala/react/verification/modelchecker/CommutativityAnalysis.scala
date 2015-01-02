package react.verification.modelchecker

import react.runtime.{RW, ScheduledTask}

case class Footprint(scope: String, read: Option[Set[String]], write: Option[Set[String]])

class CommutativityAnalysis(we: WorldExecutor, tasks: List[ScheduledTask]) {

  def intersect(v1: Option[Set[String]], v2: Option[Set[String]]): Boolean = {
    (v1, v2) match {
      case (    None,     None) => true
      case (Some(s1),     None) => !s1.isEmpty
      case (    None, Some(s1)) => !s1.isEmpty
      case (Some(s1), Some(s2)) => !(s1 intersect s2).isEmpty
    }
  }

  def indep(fp1: Footprint, fp2: Footprint): Boolean = {
    fp1.scope != fp2.scope ||
    !intersect(fp1.read, fp2.write) ||
    !intersect(fp1.write, fp2.read) ||
    !intersect(fp1.write, fp2.write)
  }

  def indep(fp1: Option[Iterable[Footprint]], fp2: Option[Iterable[Footprint]]): Boolean = {
    if (fp1.isEmpty || fp2.isEmpty) {
      false
    } else {
      val f1 = fp1.get
      val f2 = fp2.get
      f1.forall( a => f2.forall( b => indep(a,b)))
    }
  }

  def collectFootprint(we: WorldExecutor, task: RW): Option[Iterable[Footprint]] = {
    task.sendMsgsTo match {
      case Some(msgs) =>
        val rws = msgs.flatMap(m => we.getSubscriberRW(m._1))
        if (rws.forall(_.isDefined)) {
          val rws2 = rws.flatten
          val fps = rws2.map(collectFootprint(we, _))
          val fp = Footprint(task.robotID, task.read, task.written)
          fps.foldLeft(Some(List(fp)): Option[Iterable[Footprint]])( (acc, m) => m match {
            case Some(f) => acc.map(f ++ _)
            case None => None
          })
        } else {
          None
        }
      case None => None
    }
  }

  val independenceMatrix: Map[ScheduledTask,List[ScheduledTask]] = {
    val fps = tasks.map(t => (t -> t.rw.flatMap(collectFootprint(we, _)))).toMap
    tasks.map( t => {
      t -> tasks.filter( t2 => {
        val f1 = fps(t)
        val f2 = fps(t2)
        indep(f1, f2)
      })
    }).toMap
  }

  def commutes(t1: ScheduledTask, t2: ScheduledTask): Boolean = {
    independenceMatrix(t1) contains t2
  }

}
