package react.verification

import react._
import react.verification.ghost._
import react.utils._

import net.automatalib.util.automata.fsa.DFAs
import net.automatalib.util.automata.Automata
import net.automatalib.automata.fsa.impl.compact.CompactDFA
import net.automatalib.words.impl.Alphabets
import net.automatalib.words.{Word, WordBuilder}
import scala.collection.mutable.HashSet
import scala.collection.GenIterable
import java.nio.ByteBuffer

import HashStateStore._

//import java.util.concurrent.ConcurrentHashMap
//import java.util.Collections

class SafetyError(val cause: String, val suffix: List[Array[Byte]]) extends Exception("safety violation (" + cause + ")") {
}

/** Model checker options */
trait McOptions {
  /* how many ghosts steps per period */
  //var ghostSteps = 1
  var timeBound = -1
  var keepTrace = false
  var bfs = true
  var keepTransient = false
  var periodCoeff = 1
  var traceFile = ""
  var coverageFile = ""
}


class ModelChecker(world: World, exec: McExecutor, scheduler: Scheduler, opts: McOptions) {


  ///////////////////////////

  type State = Array[Byte]

  //most compact representation of the state: automaton
  protected var permanentStates = new StateStore()

  def addToPermanent(s: GenIterable[Word[Integer]]) {
    if (!s.isEmpty) {
      def union(a: CompactDFA[Integer], b: CompactDFA[Integer]) = {
        DFAs.or(a, b, permanentStates.alphabet)
      }
      val s2 = s.map(permanentStates.dfaFromWord)
      val dfa = s2.reduce( union )
      permanentStates.addDFA(dfa)
      permanentStates.minimize()
      permanentStatesStored += s.size
    }
  }

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

  protected val frontierT = new java.util.ArrayDeque[State]()
  protected def putT(s: State) = frontierT.addFirst(s)
  protected def getT: State =
    if (opts.bfs) frontierT.removeLast()
    else frontierT.removeFirst()
  protected def frontierContentT: Array[State] = {
    val a1 = frontierT.toArray( Array(Array[Byte]()) )
    val a2 = if (a1 == null) Array[State]() else a1
    a2.filter(_ != null)
  }


  ////////////////////////////////
  // saving and restoring state //
  ////////////////////////////////

  val wl = world.totalLength
  var period = -1 //TODO initialize

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


  //////////////////
  // taking steps //
  //////////////////

  def step(bp: BranchingPoint, i: Int) {
    assert(world.safe, "world unsafe before taking a step!")
    Logger("ModelChecker", LogDebug, "## start step")
    bp.act(i) //TODO add a timeout for infinite loops 
    exec.waitUntilDelivered
    Logger("ModelChecker", LogDebug, "## end step")
    statesGenerated += 1
  }

  /** executes until to next period */
  def controllerStep(s: State): Iterable[State] = {
    restoreState(s)
    if (scheduler.now >= period) {
      Nil
    } else {
      //continuous behaviour until the next discrete action
      val dt = scheduler.timeToNext.toInt
      //Logger("ModelChecker", LogNotice, "controller step: Δt = " + dt + ", t = " + scheduler.now)
      //Logger("ModelChecker", LogNotice, scheduler.toString)
      scheduler.elapse(dt)
      world.elapse(dt)
      //execute the next action
      val bp = scheduler.nextBP
      val s2 = saveState
      (0 until bp.alternatives).flatMap( i => {
        restoreState(s2)
        assert(bp.expiration == scheduler.now)
        step(bp, i)
        val s3 = saveState
        if (!world.safe) {
          throw new SafetyError("controller step", List(s2,s3))
        } else if (world.inBounds) {
          Some(s3)
        } else {
          None
        }
      })
    }
  }

  /** saturates the systems with ghosts inputs */
  def ghostStep(s: State): Iterable[State] = {
    restoreState(s)
    val bp = new BranchingPoints(world.ghosts)
    val alt = bp.alternatives
    //Logger("ModelChecker", LogNotice, "ghost steps (|branching point| = " + alt + ")")
    //Logger("ModelChecker", LogNotice, "s  = " + new RichState(s))
    (0 until alt).flatMap( i => {
      restoreState(s)
      step(bp, i)
      val s2 = saveState
      //Logger("ModelChecker", LogNotice, "s2 = " + new RichState(s2))
      if (!world.safe) {
        throw new SafetyError("ghost step", List(s2))
      } else if (world.inBounds) {
        Some(s2)
      } else {
        None
      }
    })
  }

  def safeExec[A](suffix: List[State], fct: => A ) = {
    try {
      fct
    } catch {
      case i: java.lang.InterruptedException =>
        throw i
      case s: SafetyError =>
        throw new SafetyError(s.cause, suffix ::: s.suffix)
      case exn: Throwable =>
        exn.printStackTrace
        throw new SafetyError(exn.toString, suffix) //TODO stack trace ?
    }
  }

  //the inner loop proceeds into two steps.
  //first, it generates all the reachable states by ghost perturbations (simulates user inputs, etc...)
  //then, we do the periodic controller update
  def innerLoop(s: State): Iterable[State] = {
    var local: List[State] = Nil
    var cnt = 1
    //the ghost steps
    if (!opts.keepTransient) {
      transientStates.clear()
    }
    restoreStateCompact(s)
    val s2 = saveState
    transientStates += s2
    local = s2 :: local
    transientStatesStored += 1
    putT(s2)
    while(!frontierT.isEmpty) {
      Logger("ModelChecker", LogDebug, "inner loop: ghost steps (#transient states = " + transientStates.size + ", frontier = " + frontierT.size + ")")
      if (cnt % 500 == 0) {
        Logger("ModelChecker", LogInfo, "inner loop: ghost steps (#transient states = " + transientStates.size + ", frontier = " + frontierT.size + ")")
      }
      cnt += 1 
      val st = getT
      val s2 = safeExec(List(s, st), ghostStep(st))
      for (x <- s2) {
        if (!transientStates.contains(x)) {
          transientStates += x
          local = x :: local
          transientStatesStored += 1
          putT(x)
        }
      }
    }
    //the controller step
    local foreach (rs => putT(rs.state))
    while(!frontierT.isEmpty) {
      Logger("ModelChecker", LogDebug, "inner loop: robot steps (#transient states = " + transientStates.size +  ", frontier = " + frontierT.size + ")")
      if (cnt % 500 == 0) {
        Logger("ModelChecker", LogInfo, "inner loop: robot steps (#transient states = " + transientStates.size +  ", frontier = " + frontierT.size + ")")
      }
      cnt += 1 
      val st = getT
      val s2 = safeExec(List(s, st), controllerStep(st))
      for (x <- s2) {
        if (!transientStates.contains(x)) {
          transientStates += x
          local = x :: local
          transientStatesStored += 1
          putT(x)
        }
      }
    }
    val rounded = local.flatMap( s => {
      restoreState(s.state)
      //keep only those at the period
      if (scheduler.now >= period) {
        //shift everything back to 0
        scheduler.shift(scheduler.now)
        world.round
        //minimize scheduler state
        val x = saveStateCompact
        //check if there after rounding
        if (!world.safe) {
          Logger("ModelChecker", LogInfo, "Rounding made a safe state unsafe, discarding. You should increase precision.")
          None
        } else if (!transientStates.contains(x)) {
          transientStates += x
          Some(x)
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
      Logger("ModelChecker", LogNotice, "outer loop: #permantent states = " + permanentStatesStored + ", frontier = " + frontier.size)
      if (!frontier.isEmpty) {
        val (p, s) = get
        val post = innerLoop(s).par
        val asState = post.map( s => (s -> StateStore.stateToWord(s)) )
        val news = asState.filterNot{ case (s, w) => permanentStates.contains(w) }
        val (newStates, newWords) = news.unzip
        addToPermanent(newWords)
        for (s2 <- newStates.seq) {
           if (opts.timeBound <= 0 || (p+1) * period <= opts.timeBound) {
             put(p+1, s2)
             if (opts.keepTrace) {
               predMap(s2) = s
             }
           }
        }
        true
      } else {
        false
      }
    } catch {
      case s: SafetyError =>
        Logger("ModelChecker", LogError, "Error found: " + s.cause)
        if (!s.suffix.isEmpty) {
          val last = s.suffix.last
          restoreState(last)
          Logger("ModelChecker", LogError, "last known state:")
          Logger("ModelChecker", LogError, world.toString)
          Logger("ModelChecker", LogError, world.currentState)
          if (opts.keepTrace) {
            val trace = makeTrace(s.suffix.head) ::: s.suffix.tail
            Logger("ModelChecker", LogError, "\n")
            Logger("ModelChecker", LogError, traceToString(trace))
            if (opts.traceFile != "") {
              printTraceAsSVG(opts.traceFile, trace)
            }
          }
        }
        false
    }
  }

  def init {
    Logger("ModelChecker", LogNotice, "initializing model-checker.")
    Logger("ModelChecker", LogNotice, world.stateSpaceDescription)
    val allTasks = scheduler.content ++ world.robots.flatMap(_.getAllTasks)
    period = opts.periodCoeff * Scheduler.computePeriod(allTasks)
    assert(period > 0)
    Logger("ModelChecker", LogNotice, "period = " + period)
    Logger("ModelChecker", LogNotice, scheduler.toString)
    val initState = saveStateCompact
    permanentStates.addState(initState)
    permanentStatesStored += 1
    put(0, initState)
    if(!world.safe) {
      Logger("ModelChecker", LogError, "error state reached:\n" + world.toString)
      throw new SafetyError("initial state", List(initState))
    }
    //world.grabAllLocks
    startTime = java.lang.System.currentTimeMillis()
  }

  ///////////
  // trace //
  ///////////

  val predMap = collection.mutable.HashMap[RichState,RichState]()

  def makeTrace(s: State) = {
    def process(curr: State, suffix: List[State]): List[State] = {
      if (predMap contains curr) {
        process(predMap(curr).state, curr :: suffix)
      } else {
        curr :: suffix
      }
    }
    process(s, Nil)
  }

  def traceToString(trace: List[State]) = {
    val buffer = new StringBuilder
    for ( (s, i) <- trace.zipWithIndex) {
      restoreWorldOnly(s)
      buffer.append("step: "+i+"\n")
      buffer.append(world.currentState)
      buffer.append("\n\n")
    }
    buffer.toString
  }

  def printTraceAsSVG(fileName: String, trace: List[State]) =
    react.utils.IO.writeInFile(fileName, writeTraceAsSVG(_, trace))

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

  def writeTraceAsSVG(writer: java.io.BufferedWriter, trace: List[State]) = {
    svgHeader(writer)
    for (s <- trace) {
      writeModelsAsSVG(writer, s)
    }
    svgFooter(writer)
  }


  ///////////
  // stats //
  ///////////

  var startTime = 0l
  var statesGenerated = 0l
  var transientStatesStored = 0l
  var permanentStatesStored = 0l

  def printStats {
    val dt = java.lang.System.currentTimeMillis() - startTime
    val sec = dt / 1000
    val ms = dt % 1000
    val ts = frontierContent.map(_._1)
    Logger("ModelChecker", LogNotice, "Model checker ran for " + sec + "." + ms + " seconds")
    Logger("ModelChecker", LogNotice, "  #states generated = " + statesGenerated)
    Logger("ModelChecker", LogNotice, "  #transient states = " + transientStatesStored)
    Logger("ModelChecker", LogNotice, "  #permanent states = " + permanentStatesStored)
    if (!ts.isEmpty) {
      val min = ts.min * period
      val max = ts.max * period
      Logger("ModelChecker", LogNotice, "  time horizon for the frontier ∈ [" + min + ", " + max + "]")
    }
    printCoverage
  }

  def printCoverage {
    if (opts.coverageFile != "") {
      def print(writer: java.io.BufferedWriter) {
        svgHeader(writer)
        for (s <- predMap.keys) writeModelsAsSVG(writer, s.state)
        for (s <- transientStates) writeModelsAsSVG(writer, s.state)
        for (s <- frontierContent) writeModelsAsSVG(writer, s._2)
        for (s <- frontierContentT) writeModelsAsSVG(writer, s)
        // TODO enumerate state from the StateStore ?
        svgFooter(writer)
      }
      react.utils.IO.writeInFile(opts.coverageFile, print(_))
    }
  }

}

