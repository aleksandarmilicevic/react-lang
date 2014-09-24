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
import scala.collection.parallel._
import java.nio.ByteBuffer

import HashStateStore._

class SafetyError(val cause: String, val suffix: Trace) extends Exception("safety violation (" + cause + ")") {
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
  var nbrWorlds = 4
}


object ModelChecker {
  type State = Array[Byte]
  type Label = List[String]
}

//class ModelChecker(world: World, exec: McExecutor, scheduler: Scheduler, opts: McOptions) {
class ModelChecker(worlds: Array[WorldProxy], opts: McOptions) {

  import ModelChecker._

  val world = worlds(0) //use this on when no concurrency

  val taskSupport = new ForkJoinTaskSupport(new scala.concurrent.forkjoin.ForkJoinPool(opts.nbrWorlds))

  private val qW = new java.util.concurrent.ArrayBlockingQueue[WorldProxy](worlds.length)
  for( w <- worlds ) qW.add(w)

  def isolated[A](fct: WorldProxy => A): Either[A, Throwable] = {
    val w = qW.take()
    try {
      Left(fct(w))
    } catch {
      case t: Throwable =>
        Logger("ModelChecker", LogInfo, "isolated: caught " + t)
        Right(t)
    } finally {
      qW.add(w)
    }
  }

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


  var period = -1 //TODO initialize

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
      val alts = world.controllerAlernatives
      Logger("ModelChecker", LogDebug, "|controller step| = " + alts)
      statesGenerated += alts
      val enum = (0 until alts).par
      //enum.tasksupport = taskSupport
      val successors = enum.map( i => isolated( w => w.controllerStep(dt, t, i) ) ).seq
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
    Logger("ModelChecker", LogDebug, "|ghost step| = " + alts)
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
    world.restoreStateCompact(s)
    val s2 = world.saveState
    transientStates += s2
    val t = Trace(s, List("restore from state compact") -> s2) 
    local = t :: local
    transientStatesStored += 1
    putT(t)
    while(!frontierT.isEmpty) {
      Logger("ModelChecker", LogDebug, "inner loop: ghost steps (#transient states = " + transientStates.size + ", frontier = " + frontierT.size + ")")
      if (cnt % 500 == 0) {
        Logger("ModelChecker", LogInfo, "inner loop: ghost steps (#transient states = " + transientStates.size + ", frontier = " + frontierT.size + ")")
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
      Logger("ModelChecker", LogDebug, "inner loop: robot steps (#transient states = " + transientStates.size +  ", frontier = " + frontierT.size + ")")
      if (cnt % 500 == 0) {
        Logger("ModelChecker", LogInfo, "inner loop: robot steps (#transient states = " + transientStates.size +  ", frontier = " + frontierT.size + ")")
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
          Logger("ModelChecker", LogWarning, "Rounding made a safe state unsafe, discarding. You should increase precision.")
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
      Logger("ModelChecker", LogNotice, "outer loop: #permantent states = " + permanentStatesStored + ", frontier = " + frontier.size)
      if (!frontier.isEmpty) {
        val (p, s) = get
        val post = innerLoop(s).par
        val asState = post.map( s => (s -> StateStore.stateToWord(s.stop)) )
        val news = asState.filterNot{ case (s, w) => permanentStates.contains(w) }
        val (newStates, newWords) = news.unzip
        addToPermanent(newWords)
        for (s2 <- newStates.seq) {
           if (opts.timeBound <= 0 || (p+1) * period <= opts.timeBound) {
             put(p+1, s2.stop)
             if (opts.keepTrace) {
               val t2 = s2.compact
               predMap(t2.stop) = (t2.labels.flatten, t2.start)
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
          val last = s.suffix.stop
          world.restoreState(last)
          Logger("ModelChecker", LogError, "last known state:")
          Logger("ModelChecker", LogError, world.toString)
          Logger("ModelChecker", LogError, world.currentState)
          if (opts.keepTrace) {
            val trace = makeTrace(s.suffix)
            Logger("ModelChecker", LogError, "\n")
            Logger("ModelChecker", LogError, traceToString(trace))
            if (opts.traceFile != "") {
              printTraceAsSVG(opts.traceFile, trace)
            }
          }
        }
        false
      case t: Throwable =>
        Logger("ModelChecker", LogCritical, "internal model checker error: " + t)
        throw t
    }
  }

  def init {
    Logger("ModelChecker", LogNotice, "initializing model-checker.")
    Logger("ModelChecker", LogNotice, world.stateSpaceDescription)
    val allTasks = world.allTasks
    period = opts.periodCoeff * Scheduler.computePeriod(allTasks)
    assert(period > 0)
    Logger("ModelChecker", LogNotice, "period = " + period)
    Logger("ModelChecker", LogNotice, world.schedulerToString)
    val initState = world.saveStateCompact
    permanentStates.addState(initState)
    permanentStatesStored += 1
    put(0, initState)
    if(!world.safe) {
      Logger("ModelChecker", LogError, "error state reached:\n" + world.toString)
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

  val predMap = collection.mutable.HashMap[RichState,(Label,RichState)]()

  def makeTrace(tr: Trace): Trace = {
    if (predMap contains tr.start) {
      val (l,s) = predMap(tr.start)
      makeTrace(tr.prepend(s.state, l))
    } else {
      tr
    }
  }

  def traceToString(trace: Trace) = world.traceToString(trace)

  def printTraceAsSVG(fileName: String, trace: Trace) =
    react.utils.IO.writeInFile(fileName, world.writeTraceAsSVG(_, trace))

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
        world.svgHeader(writer)
        for (s <- predMap.keys) world.writeModelsAsSVG(writer, s.state)
        for (s <- transientStates) world.writeModelsAsSVG(writer, s.state)
        for (s <- frontierContent) world.writeModelsAsSVG(writer, s._2)
        for (s <- frontierContentT) world.writeModelsAsSVG(writer, s)
        // TODO enumerate state from the StateStore ?
        world.svgFooter(writer)
      }
      react.utils.IO.writeInFile(opts.coverageFile, print(_))
    }
  }

}

