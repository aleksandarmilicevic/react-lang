package react.verification

import org.ros.RosCore
import org.ros.RosRun
import org.ros.node._
import react.utils._

import ModelChecker._

/** Encapsulate everything to explore states in parallel */
class WorldProxy(val world: World) {
    
  val scheduler: Scheduler = new Scheduler
  val exec: WorldExecutor = new WorldExecutor(world, scheduler)

  var core: RosCore = null
  var config: NodeConfiguration = null
  var rosExec: NodeMainExecutor = null

  /* start core, scheduler, exec, warm cache, ... */
  def init {
    val (co,cf) = WorldProxy.newRosCore
    core = co
    config = cf
    rosExec = DefaultNodeMainExecutor.newDefault()
    rosExec.execute(exec, config)
    var to = 0
    while(!exec.ready) {
      Logger("WorldProxy", LogNotice, "Waiting for ROS executor to start")
      Thread.sleep(1000)
      to += 1
      if (to > 5) {
        Logger.logAndThrow("WorldProxy", LogError, "Wait for ROS executor to start timed out")
      }
    }
    allTasks.foreach(scheduler.warmUpCache)
    exec.register
  }

  init

  def shutdown {
    Logger("WorldProxy", LogNotice, "shuting down core")
    rosExec.shutdown
    core.shutdown
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
    assert(world.safe, "world unsafe before taking a step!")
    Logger("ModelChecker", LogDebug, "## start step")
    val descr = bp.act(i)
    exec.waitUntilDelivered
    Logger("ModelChecker", LogDebug, "## end step")
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
    val bp = new BranchingPoints(world.ghosts)
    val alt = bp.alternatives
    if (alt > i) {
      val descr = safeExec(prefix, step(bp, i))
      getState(prefix, "ghost step " + i, descr)
    } else {
      Logger("ModelChecker", LogWarning, "ghost step " + i + " but alt = " + alt)
      None
    }
  }

  def ghostsAlternatives: Int = {
    new BranchingPoints(world.ghosts).alternatives
  }

  def controllerStep(dt: Int, prefix: Trace, i: Int): Option[Trace] = {
    val s = prefix.stop
    restoreState(s)
    elapse(dt)
    val bp = scheduler.nextBP
    assert(bp.expiration == now, "bp.expiration = " + bp.expiration + ", now = " + now)
    val descr = safeExec(prefix, step(bp, i))
    getState(prefix, "controller step " + i, descr)
  }

  def controllerAlernatives: Int = {
    val s = scheduler.saveState
    val bp = scheduler.nextBP
    val res = bp.alternatives
    scheduler.restoreState(s)
    res
  }
  
  def elapse(dt: Int) = {
    Logger("ModelChecker", LogDebug, "elapse " + dt)
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
    val tasks = scheduler.content ++ world.robots.flatMap(_.getAllTasks)
    //println(tasks.mkString("\n"))
    tasks
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
    //Logger("ModelChecker", LogWarning, "sched = " + sched.size + ", s = " + s.size)
    sched
  }

  def saveState: State = {
    val w = world.getCurrentState
    val s = scheduler.saveState
    val full = w ++ s
    //Logger("ModelChecker", LogWarning, "wl = " + wl + ", w = " + w.size + ", s = " + s.size + ", full = " + full.size)
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
      buffer.append("step "+i+": " + t.mkString(", ") + "\n")
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
    for (b <- world.envBoxes) {
      b.writeAsSVG(writer)
      writer.newLine
    }
  }

  def svgFooter(writer: java.io.BufferedWriter) {
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

  private val port = new java.util.concurrent.atomic.AtomicInteger(11111)

  def newRosCore = {
    val p = port.getAndIncrement
    val core = RosCore.newPrivate(p)
    Logger("WorldProxy", LogNotice, "starting ROS core on " + p)
    core.start()
    try {
      core.awaitStart()
    } catch {
      case e: Exception => throw new RuntimeException(e)
    }
    Logger("WorldProxy", LogNotice, "core started on " + p)
    val config = NodeConfiguration.newPrivate(core.getUri())
    config.setNodeName("ReactVerifier"+p)
    (core, config)
  }

}
