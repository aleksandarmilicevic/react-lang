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


class ModelChecker(world: World, scheduler: Scheduler) {

  ///////////////////////////
  // Model checker options //
  ///////////////////////////

  /* how many ghosts steps per period */
  //var ghostSteps = 1

  var timeBound = -1

  var keepTrace = false //require much more memory

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

  protected val frontier = new java.util.ArrayDeque[State]()
  protected def put(s: State) = frontier.addFirst(s)
  protected def get: State = frontier.removeLast()

  //to represent the transient states between the round
  protected val transientStates = new HashStateStore()

  protected val frontierT = new java.util.ArrayDeque[State]()
  protected def putT(s: State) = frontierT.addFirst(s)
  protected def getT: State = frontierT.removeLast()


  ////////////////////////////////
  // saving and restoring state //
  ////////////////////////////////

  val wl = world.totalLength
  var defaultSchedulerState: State = null //TODO initialize
  var period = -1 //TODO initialize

  def stripSchedulerState(s: State): State = {
    s.slice(0, wl)
  }
  
  def getSchedulerState(s: State): State = {
    val sched = s.drop(wl)
    //Logger("ModelChecker", LogWarning, "sched = " + sched.size + ", s = " + s.size)
    sched
  }

  def addDefaultSchedulerState(s: State): State = {
    s ++ defaultSchedulerState
  }

  def saveStateWithScheduler: State = {
    val w = world.getCurrentState
    val s = scheduler.saveState
    val full = w ++ s
    //Logger("ModelChecker", LogWarning, "wl = " + wl + ", w = " + w.size + ", s = " + s.size + ", full = " + full.size)
    full
  }

  def saveStateWithoutScheduler: State = {
    world.getCurrentState
  }

  def restoreStateWithScheduler(s: State) {
    //Logger("ModelChecker", LogNotice, scheduler.toString)
    world.restoreState(s)
    scheduler.restoreState(getSchedulerState(s))
  }

  def restoreStateWithoutScheduler(s: State) {
    world.restoreState(s)
  }


  //////////////////
  // taking steps //
  //////////////////

  def step(bp: BranchingPoint, i: Int) {
    bp.act(i) //TODO add a timeout for infinite loops 
    world.waitUntilStable
    statesGenerated += 1
  }

  /** executes until to next period */
  def controllerStep(s: State): Iterable[State] = {
    restoreStateWithScheduler(s)
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
      val s2 = saveStateWithScheduler
      for (i <- 0 until bp.alternatives) yield {
        restoreStateWithScheduler(s2)
        step(bp, i)
        val s3 = saveStateWithScheduler
        if (!world.safe) {
          Logger("ModelChecker", LogError, "error state reached:\n" + world.toString)
          throw new SafetyError("controller step", List(s2,s3))
        } else {
          s3
        }
      }
    }
  }

  /** saturates the systems with ghosts inputs */
  def ghostStep(s: State): Iterable[State] = {
    restoreStateWithScheduler(s)
    val bp = new BranchingPoints(world.ghosts)
    val alt = bp.alternatives
    //Logger("ModelChecker", LogNotice, "ghost steps (|branching point| = " + alt + ")")
    //Logger("ModelChecker", LogNotice, "s  = " + new RichState(s))
    for(i <- 0 until alt) yield {
      restoreStateWithScheduler(s)
      step(bp, i)
      val s2 = saveStateWithScheduler
      //Logger("ModelChecker", LogNotice, "s2 = " + new RichState(s2))
      if (!world.safe) {
        Logger("ModelChecker", LogError, "error state reached:\n" + world.toString)
        throw new SafetyError("ghost step", List(s2))
      } else {
        s2
      }
    }
  }

  def safeExec[A](suffix: List[State], fct: => A ) = {
    try {
      fct
    } catch {
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
    var cnt = 1
    //the ghost steps
    transientStates.clear()
    val s2 = addDefaultSchedulerState(s) //TODO we should also take into account the rounding
    transientStates += s2
    transientStatesStored += 1
    putT(s2)
    while(!frontierT.isEmpty) {
      Logger("ModelChecker", LogDebug, "inner loop: ghost steps (#transient states = " + transientStates.size + ", frontier = " + frontierT.size + ")")
      if (cnt % 100 == 0) {
        Logger("ModelChecker", LogInfo, "inner loop: ghost steps (#transient states = " + transientStates.size + ", frontier = " + frontierT.size + ")")
      }
      cnt += 1 
      val st = getT
      val s2 = safeExec(List(s, st), ghostStep(st))
      for (x <- s2) {
        if (!transientStates.contains(x)) {
          transientStates += x
          transientStatesStored += 1
          putT(x)
        }
      }
    }
    //the controller step
    transientStates foreach (rs => putT(rs.state))
    while(!frontierT.isEmpty) {
      Logger("ModelChecker", LogDebug, "inner loop: robot steps (#transient states = " + transientStates.size +  ", frontier = " + frontierT.size + ")")
      if (cnt % 1000 == 0) {
        Logger("ModelChecker", LogInfo, "inner loop: robot steps (#transient states = " + transientStates.size +  ", frontier = " + frontierT.size + ")")
      }
      cnt += 1 
      val st = getT
      val s2 = safeExec(List(s, st), controllerStep(st))
      for (x <- s2) {
        if (!transientStates.contains(x)) {
          transientStates += x
          transientStatesStored += 1
          putT(x)
        }
      }
    }
    val stateLst = transientStates.toList.map(_.state)
    transientStates.clear
    stateLst.flatMap[State, List[State]]( s => {
      restoreStateWithScheduler(s)
      //keep only those at the period
      if (scheduler.now >= period) {
        //shift everything back to 0
        scheduler.shift(scheduler.now)
        world.round
        // remove scheduler state
        Some(saveStateWithoutScheduler)
      } else None
    })
  }
  
  def oneStep = {
    try {
      Logger("ModelChecker", LogNotice, "outer loop: #permantent states = " + permanentStatesStored + ", frontier = " + frontier.size)
      if (!frontier.isEmpty) {
        val s = get
        val post = innerLoop(s).par
        val asState = post.map( s => (s -> StateStore.stateToWord(s)) )
        val news = asState.filterNot{ case (s, w) => permanentStates.contains(w) }
        val (newStates, newWords) = news.unzip
        addToPermanent(newWords)
        for (s2 <- newStates.seq) {
           put(s2)
           if (keepTrace) {
             predMap(s2) = s
           }
        }
        inPeriod -= 1
        if (inPeriod <= 0) {
          numberOfPeriod += 1
          inPeriod = frontier.size
        }
     
        if (timeBound > 0 && period*numberOfPeriod > timeBound) {
          false
        } else {
          true
        }
      } else {
        false
      }
    } catch {
      case s: SafetyError =>
        Logger("ModelChecker", LogError, "Error found: " + s.cause)
        if (!s.suffix.isEmpty) {
          val last = s.suffix.last
          restoreStateWithScheduler(last)
          Logger("ModelChecker", LogError, "last known state: " + world)
          if (keepTrace) {
            val trace = makeTrace(s.suffix.head) ::: s.suffix.tail
            Logger("ModelChecker", LogError, "trace:\n  " + trace.mkString("\n  ")) //TODO decent printing
          }
        }
        false
    }
  }

  def init {
    Logger("ModelChecker", LogNotice, "initializing model-checker.")
    Logger("ModelChecker", LogNotice, world.stateSpaceDescription)
    defaultSchedulerState = scheduler.saveState
    period = scheduler.computePeriod
    Logger("ModelChecker", LogNotice, "period = " + period)
    Logger("ModelChecker", LogNotice, scheduler.toString)
    val initState = saveStateWithoutScheduler
    permanentStates.addState(initState)
    permanentStatesStored += 1
    put(initState)
    if(!world.safe) {
      Logger("ModelChecker", LogError, "error state reached:\n" + world.toString)
      throw new SafetyError("initial state", List(initState))
    }
    world.grabAllLocks
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


  ///////////
  // stats //
  ///////////

  var startTime = 0l
  var statesGenerated = 0l
  var transientStatesStored = 0l
  var permanentStatesStored = 0l
  var inPeriod = 1
  var numberOfPeriod = 0l

  def printStats {
    val dt = java.lang.System.currentTimeMillis() - startTime
    val sec = dt / 1000
    val ms = dt % 1000
    Logger("ModelChecker", LogNotice, "Model checker ran for " + sec + "." + ms + " seconds")
    Logger("ModelChecker", LogNotice, "  #states generated = " + statesGenerated)
    Logger("ModelChecker", LogNotice, "  #transient states = " + transientStatesStored)
    Logger("ModelChecker", LogNotice, "  #permanent states = " + permanentStatesStored)
    Logger("ModelChecker", LogNotice, "  time horizon ≈ " + numberOfPeriod * period)
  }

  def printCoverage {
    // TODO enumerate state from the StateStore ...
  }

}

