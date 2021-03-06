package react.verification.modelchecker

import react._
import react.verification._
import react.verification.ghost._
import react.utils._

import scala.collection.GenIterable
import scala.collection.parallel._

import dzufferey.utils.LogLevel._
import dzufferey.utils.Logger

import HashStateStore._

class SafetyError(val cause: String, val suffix: Trace) extends Exception("safety violation (" + cause + ")") {
}


object ModelChecker {
  type State = Array[Byte]
  type Label = List[String]
}

//class ModelChecker(world: World, exec: McExecutor, scheduler: Scheduler, opts: McOptions) {
class ModelChecker(worlds: Array[WorldProxy], opts: McOptions) {

  import ModelChecker._

  val world = worlds(0) //use this on when no concurrency

  //val taskSupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(opts.nbrWorlds))

  private val qW = new java.util.concurrent.ArrayBlockingQueue[WorldProxy](worlds.length)
  for( w <- worlds ) qW.add(w)

  def isolated[A](fct: WorldProxy => A): Either[A, Throwable] = {
    val w = qW.take()
    try {
      Left(fct(w))
    } catch {
      case t: Throwable =>
        Logger("ModelChecker", Info, "isolated: caught " + t)
        Right(t)
    } finally {
      qW.add(w)
    }
  }

  //most compact representation of the state: automaton
  protected var permanentStates = new HashDfaStateStore() //de we even need a DFA, currently seems much worse

  protected val frontier = new java.util.ArrayDeque[(Int, State)]()
  protected def put(p: Int, s: State) = frontier.addFirst(p -> s)
  protected def get: (Int, State) =
    if (opts.bfs) frontier.removeLast()
    else frontier.removeFirst()
  protected def frontierContent: Array[(Int, State)] = {
    val a1 = frontier.toArray( Array(0 -> Array[Byte]()) )
    val a2 = if (a1 == null) Array[(Int, State)]() else a1
    a2.filter(_ != null)
  }

  //to represent the transient states between the round
  protected val transientStates = new HashStateStore()

  //TODO refactor to allow other search heuristics
  protected val frontierT = new java.util.ArrayDeque[Trace]()
  protected def putT(t: Trace) = frontierT.addFirst(t)
  protected def getT: Trace =
    if (opts.bfs) frontierT.removeLast()
    else frontierT.removeFirst()
  protected def frontierContentT: Array[State] = {
    val a1: Array[Trace] = frontierT.toArray( Array[Trace]() )
    val a2: Array[Trace] = if (a1 == null) Array[Trace]() else a1
    a2.filter(_ != null).flatMap(_.states)
  }


  var period = -1 //initialized later

  //////////////////
  // taking steps //
  //////////////////

  /** executes until to next period */
  def controllerStep(t: Trace): Iterable[Trace] = {
    val s = t.stop
    world.restoreState(s)
    if (world.now >= period) {
      Nil
    } else {
      val dt = world.timeToNext.toInt
      val alts = world.controllerAlernatives(dt)
      Logger("ModelChecker", Debug, "|controller step| = " + alts)
      statesGenerated += alts
      val enum = (0 until opts.nbrWorlds).par
      val successors = enum.map( i => isolated( w => w.controllerStep(dt, t, i, opts.nbrWorlds) ) ).seq
      successors.flatMap{
        case Left(l) => l
        case Right(t) => throw t
      }
    }
  }

  /** saturates the systems with ghosts inputs */
  def ghostStep(t: Trace): Iterable[Trace] = {
    val s = t.stop
    world.restoreState(s)
    val alts = world.ghostsAlternatives
    Logger("ModelChecker", Debug, "|ghost step| = " + alts)
    statesGenerated += alts
    val enum = (0 until alts).par
    //enum.tasksupport = taskSupport
    val successors = enum.map( i => isolated( w => w.ghostStep(t, i)) ).seq
    successors.flatMap{
      case Left(l) => l
      case Right(t) => throw t
    }
  }

  //the inner loop proceeds into two steps.
  //first, it generates all the reachable states by ghost perturbations (simulates user inputs, etc...)
  //then, we do the periodic controller update
  def innerLoop(s: State): Iterable[Trace] = {
    var local: List[Trace] = Nil
    var cnt = 1
    //the ghost steps
    if (!opts.keepTransient) {
      transientStates.clear()
    }
    val s2s =
      if (opts.withConcretize) {
        world.concretizeCompactState(s)
      } else {
        world.restoreStateCompact(s)
        List(world.saveState)
      }
    statesGenerated += s2s.size - 1
    for (s2 <- s2s) {
      transientStates += s2
      val t = Trace(s, List("restore from state compact") -> s2) 
      local = t :: local
      transientStatesStored += 1
      putT(t)
    }
    while(!frontierT.isEmpty) {
      Logger("ModelChecker", Debug, "inner loop: ghost steps (#transient states = " + transientStates.size + ", frontier = " + frontierT.size + ")")
      if (cnt >= 500) {
        Logger("ModelChecker", Info, "inner loop: ghost steps (#transient states = " + transientStates.size + ", frontier = " + frontierT.size + ")")
        cnt = 0
      }
      cnt += 1 
      val tr = getT
      val s2 = ghostStep(tr)
      for (t2 <- s2) {
        val x = t2.stop
        if (!transientStates.contains(x)) {
          transientStates += x
          local = t2 :: local
          transientStatesStored += 1
          putT(t2)
        }
      }
    }
    //the controller step
    local foreach putT
    while(!frontierT.isEmpty) {
      Logger("ModelChecker", Debug, "inner loop: robot steps (#transient states = " + transientStates.size +  ", frontier = " + frontierT.size + ")")
      if (cnt >= 500) {
        Logger("ModelChecker", Info, "inner loop: robot steps (#transient states = " + transientStates.size +  ", frontier = " + frontierT.size + ")")
        cnt = 0
      }
      cnt += 1 
      val tr = getT
      val s2 = controllerStep(tr)
      for ( t2 <- s2) {
        val x = t2.stop
        if (!transientStates.contains(x)) {
          transientStates += x
          local = t2 :: local
          transientStatesStored += 1
          putT(t2)
        }
      }
    }
    val rounded = local.flatMap( s => {
      world.restoreState(s.stop)
      //keep only those at the period
      if (world.now >= period) {
        //shift everything back to 0
        world.prepareForCompactStore
        val x = world.saveStateCompact
        val t = s.append(List("compact"), x)
        //check if there after rounding
        if (!world.safe) {
          Logger("ModelChecker", Warning, "Rounding made a safe state unsafe, discarding. You should increase precision.")
          None
        } else if (!transientStates.contains(x)) {
          transientStates += x
          Some(t)
        } else {
          None
        }
      } else None
    })
    if (!opts.keepTransient) {
      transientStates.clear
    }
    rounded
  }
  
  def oneStep = {
    try {
      Logger("ModelChecker", Notice, "outer loop: #permantent states = " + permanentStatesStored + ", frontier = " + frontier.size)
      if (!frontier.isEmpty) {
        val (p, s) = get
        val post = innerLoop(s)
        val news = post.filterNot( trace => permanentStates.contains(trace.stop))
        for (s2 <- news) {
          permanentStates.add(s2.stop)
          permanentStatesStored += 1
          if (opts.timeBound <= 0 || (p+1) * period <= opts.timeBound) {
            put(p+1, s2.stop)
            predMap.add(s2)
          }
        }
        true
      } else {
        false
      }
    } catch {
      case s: SafetyError =>
        Logger("ModelChecker", Error, "Error found: " + s.cause)
        if (!s.suffix.isEmpty) {
          val last = s.suffix.stop
          world.restoreState(last)
          Logger("ModelChecker", Error, "last known state:")
          Logger("ModelChecker", Error, world.toString)
          Logger("ModelChecker", Error, world.currentState)
          val trace = makeTrace(s.suffix)
          Logger("ModelChecker", Error, "\n")
          Logger("ModelChecker", Error, traceToString(trace))
          if (opts.traceFile != "") {
            printTraceAsSVG(opts.traceFile, trace)
          }
        }
        false
      case t: Throwable =>
        Logger("ModelChecker", Critical, "model checker error: " + t + "\n  " + t.getStackTrace.mkString("\n  "))
        false
    }
  }

  def init {
    Logger.disallow("Typer")
    Logger("ModelChecker", Notice, "initializing model-checker.")
    Logger("ModelChecker", Notice, world.stateSpaceDescription)
    val allTasks = world.allTasks
    period = opts.periodCoeff * Scheduler.computePeriod(allTasks)
    assert(period > 0)
    Logger("ModelChecker", Notice, "period = " + period)
    Logger("ModelChecker", Notice, world.schedulerToString)
    val initState = world.saveStateCompact
    permanentStates.add(initState)
    permanentStatesStored += 1
    put(0, initState)
    if(!world.safe) {
      Logger("ModelChecker", Error, "error state reached:\n" + world.toString)
      throw new SafetyError("initial state", Trace(initState))
    }
    val ri = new RichState(initState)
    for (i <- 0 until opts.nbrWorlds) {
      isolated(w => {
        val rs = new RichState(w.saveStateCompact)
        Logger.assert(ri == rs, "ModelChecker", "initial state different accross worlds\n" + ri + "\n" + rs)
      }) match {
        case Right(t) => throw t
        case _ => ()
      }
    }
    startTime = java.lang.System.currentTimeMillis()
  }

  ///////////
  // trace //
  ///////////

  val predMap = new TraceStore

  def makeTrace(tr: Trace): Trace = predMap.makeTrace(tr)

  def traceToString(trace: Trace) = world.traceToString(trace)

  def printTraceAsSVG(fileName: String, trace: Trace) =
    dzufferey.utils.IO.writeInFile(fileName, world.writeTraceAsSVG(_, trace))

  ///////////
  // stats //
  ///////////

  var startTime = 0l
  var statesGenerated = 0l
  var transientStatesStored = 0l
  var permanentStatesStored = 0l

  /** this also free some resources, call only when you don't need the modelchecker anymore */
  def printStats {
    val dt = java.lang.System.currentTimeMillis() - startTime
    val sec = dt / 1000
    val ms = dt % 1000
    val ts = frontierContent.map(_._1)
    Logger("ModelChecker", Notice, "Model checker ran for " + sec + "." + ms + " seconds")
    Logger("ModelChecker", Notice, "  #states generated = " + statesGenerated)
    Logger("ModelChecker", Notice, "  #transient states = " + transientStatesStored)
    Logger("ModelChecker", Notice, "  #permanent states = " + permanentStatesStored)
    if (!ts.isEmpty) {
      val min = ts.min * period
      val max = ts.max * period
      Logger("ModelChecker", Notice, "  time horizon for the frontier ∈ [" + min + ", " + max + "]")
    }
    printCoverage
    predMap.clean
  }

  def printCoverage {
    if (opts.coverageFile != "") {
      def print(writer: java.io.BufferedWriter) {
        world.svgHeader(writer)
        for (s <- predMap.states) world.writeModelsAsSVG(writer, s.state)
        for (s <- transientStates) world.writeModelsAsSVG(writer, s.state)
        for (s <- frontierContent) world.writeModelsAsSVG(writer, s._2)
        for (s <- frontierContentT) world.writeModelsAsSVG(writer, s)
        world.svgFooter(writer)
      }
      dzufferey.utils.IO.writeInFile(opts.coverageFile, print(_))
    }
  }

}

