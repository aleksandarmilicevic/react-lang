package react.verification.modelchecker

import org.ros.RosCore
import org.ros.RosRun
import org.ros.node._

import react.utils._
import react.verification._

import dzufferey.utils.LogLevel._
import dzufferey.utils.Logger


import ModelChecker._

/** Encapsulate everything to explore states in parallel */
class WorldProxy(val world: World, opts: McOptions) {
    
  val scheduler: Scheduler = new Scheduler
  val exec: WorldExecutor = new WorldExecutor(world, scheduler, opts.bypassROS)

  var core: RosCore = null
  var config: NodeConfiguration = null
  var rosExec: NodeMainExecutor = null

  /* start core, scheduler, exec, warm cache, ... */
  def init {
    val (co,cf) = WorldProxy.newRosCore
    core = co
    //co.getMasterServer.setMaxThreads(10)
    config = cf
    rosExec = DefaultNodeMainExecutor.newDefault()//(WorldProxy.pool)
    rosExec.execute(exec, config)
    var to = 0
    while(!exec.ready) {
      Logger("WorldProxy", Notice, "Waiting for ROS executor to start")
      Thread.sleep(1000)
      to += 1
      if (to > 5) {
        Logger.logAndThrow("WorldProxy", Error, "Wait for ROS executor to start timed out")
      }
    }
    exec.register
    allTasks.foreach(scheduler.warmUpCache)
    Logger("WorldProxy", Info, "scheduler cache = " + scheduler.cache.size)
  }

  init

  def shutdown {
    Logger("WorldProxy", Notice, "shuting down core")
    rosExec.shutdown
    core.shutdown
    //pool.shutdownNow
    rosExec = null
    core = null
    config = null
  }

  override def toString = { 
    world.toString
  }

  //////////////////
  // taking steps //
  //////////////////

  protected def safeExec[A](suffix: Trace, fct: => A) = {
    try {
      fct //TODO add a timeout for infinite loops 
    } catch {
      case i: java.lang.InterruptedException =>
        throw i
      case s: SafetyError =>
        throw new SafetyError(s.cause, suffix concat s.suffix)
      case exn: Throwable =>
        exn.printStackTrace
        throw new SafetyError(exn.toString, suffix) //TODO stack trace ?
    }
  }

  protected def step(bp: BranchingPoint, i: Int): Label = {
    assert(world.safe, "world unsafe before taking a step!\n" + world.currentState)
    Logger("ModelChecker", Debug, "## start step")
    val descr = bp.act(i)
    exec.waitUntilDelivered
    Logger("ModelChecker", Debug, "## end step")
    descr
  }

  protected def getState(prefix: Trace, what: String, descr: Label) = {
    val s2 = saveState
    val trace = prefix.append(descr, s2)
    if (!world.safe) {
      throw new SafetyError(what + ": " + descr.mkString(", "), trace)
    } else if (world.inBounds) {
      Some(trace)
    } else {
      None
    }
  }

  def ghostStep(prefix: Trace, i: Int): Option[Trace] = {
    val s = prefix.stop
    restoreState(s)
    val bp = new SumBranchingPoint(world.ghosts)
    val alt = bp.alternatives
    if (alt > i) {
      val descr = safeExec(prefix, step(bp, i))
      getState(prefix, "ghost step " + i, descr)
    } else {
      Logger("ModelChecker", Warning, "ghost step " + i + " but alt = " + alt)
      None
    }
  }

  def ghostsAlternatives: Int = {
    new SumBranchingPoint(world.ghosts).alternatives
  }

  def controllerStep(dt: Int, prefix: Trace, baseIndex: Int, period: Int): Iterable[Trace] = {
    val s = prefix.stop
    restoreState(s)

    var i = baseIndex
    val bpw = world.elapseBP(dt)
    var bps = scheduler.nextBP(exec)
    val altw = bpw.alternatives
    val alts = bps.alternatives

    var iw = i / alts
    var is = i % alts

    var acc = List[Trace]()

    while (iw < altw) {
      
      Logger("ModelChecker", Debug, "elapse(" + dt + ") → " + iw + "/" + altw)
      restoreState(s)
      bpw.act(iw)
      scheduler.elapse(dt)
      if (!world.safe) {
        throw new SafetyError("elapse : " + dt, prefix.append(List("elapse : " + dt), saveState))
      }
      bps = scheduler.nextBP(exec)
      assert(bps.expiration == now, "bp.expiration = " + bps.expiration + ", now = " + now)
      val sDt = saveState

      while(is < alts) {
        Logger("ModelChecker", Debug, "controller step " + is)
        restoreState(sDt)
        val descr = safeExec(prefix, step(bps, is))
        getState(prefix, "controller step " + is, descr) match {
          case Some(t) => 
            acc = t :: acc
          case None => ()
        }
        is += period
      }
      iw += is / alts
      is = is % alts
    }

    acc
  }

  def controllerAlernatives(dt: Int): Int = {
    val s = scheduler.saveState
    val bpw = world.elapseBP(dt)
    val bps = scheduler.nextBP(exec)
    val res = bpw.alternatives * bps.alternatives
    scheduler.restoreState(s)
    res
  }
  
  def elapse(dt: Int) = {
    Logger("ModelChecker", Debug, "elapse " + dt)
    scheduler.elapse(dt)
    world.elapse(dt)
  }

  ///////////
  // world //
  ///////////

  def safe = world.safe

  def inBounds = world.inBounds

  def worldAgnostic: Boolean = world.worldAgnostic
  
  def currentState = world.currentState

  def stateSpaceDescription = world.stateSpaceDescription


  ///////////////
  // scheduler //
  ///////////////

  def now = scheduler.now

  def timeToNext = scheduler.timeToNext
    
  def allTasks = {
    scheduler.content ++ world.robots.flatMap(_.getAllTasks)
  }

  def schedulerToString = scheduler.toString

  ////////////////////////////////
  // saving and restoring state //
  ////////////////////////////////

  val wl = world.totalLength

  def prepareForCompactStore = {
    scheduler.shift(scheduler.now)
    world.round
  }

  def stripSchedulerState(s: State): State = {
    s.slice(0, wl)
  }
  
  def getSchedulerState(s: State): State = {
    val sched = s.drop(wl)
    //Logger("ModelChecker", Warning, "sched = " + sched.size + ", s = " + s.size)
    sched
  }

  def saveState: State = {
    val w = world.getCurrentState
    val s = scheduler.saveState
    val full = w ++ s
    //Logger("ModelChecker", Warning, "wl = " + wl + ", w = " + w.size + ", s = " + s.size + ", full = " + full.size)
    full
  }

  def saveStateCompact: State = {
    val w = world.getCurrentState
    val s = scheduler.compactState
    val full = w ++ s
    full
  }
  
  def restoreWorldOnly(s: State) {
    world.restoreState(s)
  }

  def restoreState(s: State) {
    world.restoreState(s)
    scheduler.restoreState(getSchedulerState(s))
  }

  def restoreStateCompact(s: State) {
    world.restoreState(s)
    scheduler.restoreCompact(getSchedulerState(s))
  }

  /** Take a rounded state and returns states within that abstract state
   *  at the boundaries of the abstract state and in the center.
   *  TODO this could also be done in parallel
   */
  def concretizeCompactState(s: State): List[State] = {
    restoreStateCompact(s)
    //println(world.toString)
    val it = world.concretizations
    var acc: List[State] = Nil
    //println("XXX")
    while (it.hasNext) {
      it.next()
      //println("# " + acc.size)
      //println(world.toString)
      acc = saveState :: acc
    }
    //println("YYY")
    assert(!acc.isEmpty)
    Logger("WorldProxy", Debug, "concretization generated: " + acc.size + " states.")
    acc
  }


  /////////////////////
  // pretty printing //
  /////////////////////

  def traceToString(trace: Trace) = {
    val buffer = new StringBuilder
    restoreWorldOnly(trace.start)
    buffer.append("initial state\n")
    buffer.append(currentState)
    for ( ((t,s), i) <- trace.zipWithIndex) {
      restoreWorldOnly(s)
      buffer.append("step "+i+":\n  " + t.mkString("\n  ") + "\n")
      buffer.append(currentState)
      buffer.append("\n\n")
    }
    buffer.toString
  }
  
  def writeModelsAsSVG(writer: java.io.BufferedWriter, s: State) = {
    restoreWorldOnly(s)
    for ( (m,i) <- world.models.zipWithIndex) {
      val c = colors(i % colors.length)
      m.writeAsSVG(writer, c)
    }
  }

  
  def svgHeader(writer: java.io.BufferedWriter) {
    val _w = world.xMax - world.xMin
    val _h = world.yMax - world.yMin
    val w = "width=\""+ _w * 50 +"\""
    val h = "height=\""+ _h * 50 +"\""
    val vb = "viewBox=\""+(world.xMin-1)+" "+(world.yMin-1)+" "+(_w+2)+" "+(_h+2)+"\""
    writer.write("<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\" "+w+" "+h+" "+vb+" >")
    writer.newLine
    writer.write("<g transform=\"scale(1, -1)\" >")
    writer.newLine
    for (b <- world.envBoxes) {
      b.writeAsSVG(writer)
      writer.newLine
    }
  }

  def svgFooter(writer: java.io.BufferedWriter) {
    writer.write("</g>")
    writer.newLine
    writer.write("</svg>")
    writer.newLine
  }
  
  private val colors = Array("blue", "red", "yellow", "green", "cyan", "magenta")

  def writeTraceAsSVG(writer: java.io.BufferedWriter, trace: Trace) = {
    svgHeader(writer)
    for (s <- trace.states) {
      writeModelsAsSVG(writer, s)
    }
    svgFooter(writer)
  }


}


object WorldProxy {

  //val pool = new org.ros.concurrent.DefaultScheduledExecutorService

  private val port = new java.util.concurrent.atomic.AtomicInteger(11111)

  def newRosCore = {
    val p = port.getAndIncrement
    val core = RosCore.newPrivate(p)
    Logger("WorldProxy", Notice, "starting ROS core on " + p)
    core.start()
    try {
      core.awaitStart()
    } catch {
      case e: Exception => throw new RuntimeException(e)
    }
    Logger("WorldProxy", Notice, "core started on " + p)
    val config = NodeConfiguration.newPrivate(core.getUri())
    config.setNodeName("ReactVerifier"+p)
    (core, config)
  }

}
