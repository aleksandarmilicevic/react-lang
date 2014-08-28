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

class SafetyError(step: String, suffix: List[Array[Byte]]) extends Exception("safety violation (" + step + ")") {
}


class ModelChecker(world: World, scheduler: Scheduler) {

  ///////////////////////////
  // Model checker options //
  ///////////////////////////

  /* how many ghosts steps per period */
  //var ghostSteps = 1

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
    }
  }

  protected val frontier = new java.util.ArrayDeque[State]()
  protected def put(s: State) = frontier.addFirst(s)
  protected def get: State = frontier.removeLast() //TODO last/first for BFS or DFS

  //to represent the transient states between the round
  protected val transientStates = new HashStateStore()

  protected val frontierT = new java.util.ArrayDeque[State]()
  protected def putT(s: State) = frontierT.addFirst(s)
  protected def getT: State = frontierT.removeLast() //TODO last/first for BFS or DFS


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
    s.drop(wl)
  }

  def addDefaultSchedulerState(s: State): State = {
    s ++ defaultSchedulerState
  }

  def saveStateWithScheduler: State = {
    world.getCurrentState ++ scheduler.saveState
  }

  def saveStateWithoutScheduler: State = {
    world.getCurrentState
  }

  def restoreStateWithScheduler(s: State) {
    world.restoreState(s)
    scheduler.restoreState(getSchedulerState(s))
  }

  def restoreStateWithoutScheduler(s: State) {
    world.restoreState(s)
  }


  //////////////////
  // taking steps //
  //////////////////

  /** executes until to next period */
  def controllerStep(s: State): Iterable[State] = {
    restoreStateWithScheduler(s)
    if (scheduler.now >= period) {
      Nil
    } else {
      //continuous behaviour until the next discrete action
      val dt = scheduler.timeToNext.toInt
      Logger("ModelChecker", LogNotice, "controller step: Δt = " + dt + ", t = " + scheduler.now)
      Logger("ModelChecker", LogNotice, scheduler.toString)
      scheduler.elapse(dt)
      for (m <- world.models) {
        m.elapse(dt)
      }
      world.dispatchBoxes
      //execute the next action
      val s2 = saveStateWithScheduler
      val bp = scheduler.nextBP
      for (i <- 0 until bp.alternatives) yield {
        restoreStateWithScheduler(s2)
        bp.act(i) //TODO add a timeout for infinite loops 
        world.waitUntilStable
        val s3 = saveStateWithScheduler
        if (!world.safe) {
          Logger("ModelChecker", LogError, "error state reached:\n" + world.toString)
          throw new SafetyError("controller step", List(s,s2,s3))
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
    Logger("ModelChecker", LogNotice, "ghost steps (|branching point| = " + alt + ")")
    //Logger("ModelChecker", LogNotice, "s  = " + new RichState(s))
    for(i <- 0 until alt) yield {
      restoreStateWithScheduler(s)
      bp.act(i) //TODO add a timeout for infinite loops 
      world.waitUntilStable
      val s2 = saveStateWithScheduler
      //Logger("ModelChecker", LogNotice, "s2 = " + new RichState(s2))
      if (!world.safe) {
        Logger("ModelChecker", LogError, "error state reached:\n" + world.toString)
        throw new SafetyError("ghost step", List(s,s2))
      } else {
        s2
      }
    }
  }

  //the inner loop proceeds into two steps.
  //first, it generates all the reachable states by ghost perturbations (simulates user inputs, etc...)
  //then, we do the periodic controller update
  def innerLoop(s: State): Iterable[State] = {
    //the ghost steps
    transientStates.clear()
    val s2 = addDefaultSchedulerState(s)
    transientStates += s2
    putT(s2)
    while(!frontierT.isEmpty) {
      Logger("ModelChecker", LogNotice, "inner loop: ghost steps (#transient states = " + transientStates.size + ", frontier = " + frontierT.size + ")")
      val s = getT
      val s2 = ghostStep(s)
      for (x <- s2) {
        if (!transientStates.contains(x)) {
          transientStates += x
          putT(x)
        }
      }
    }
    //the controller step
    transientStates foreach (rs => putT(rs.state))
    while(!frontierT.isEmpty) {
      Logger("ModelChecker", LogNotice, "inner loop: robot steps (#transient states = " + transientStates.size +  ", frontier = " + frontierT.size + ")")
      val s = getT
      val s2 = controllerStep(s)
      for (x <- s2) {
        if (!transientStates.contains(x)) {
          transientStates += x
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
        // remove scheduler state
        Some(saveStateWithoutScheduler)
      } else None
    })
  }
  
  def outerLoop = {
    Logger("ModelChecker", LogNotice, "outer loop (|permantent states|: " + permanentStates.size + ", frontier = " + frontier.size + ")")
    if (!frontier.isEmpty) {
      val s = get
      val post = innerLoop(s).par
      val asState = post.map( s => (s -> StateStore.stateToWord(s)) )
      val news = asState.filterNot{ case (s, w) => permanentStates.contains(w) }
      val (newStates, newWords) = news.unzip
      addToPermanent(newWords)
      newStates.seq foreach put
    }
  }

  def init {
    Logger("ModelChecker", LogNotice, "initializing model-checker.")
    Logger("ModelChecker", LogNotice, world.stateSpaceDescription)
    defaultSchedulerState = scheduler.saveState
    period = scheduler.computePeriod
    Logger("ModelChecker", LogNotice, "period = " + period)
    Logger("ModelChecker", LogNotice, scheduler.toString)
    val initState = saveStateWithScheduler
    permanentStates.addState(initState)
    put(initState)
    if(!world.safe) {
      Logger("ModelChecker", LogError, "error state reached:\n" + world.toString)
      throw new SafetyError("initial state", List(initState))
    }
  }

  def oneStep = {
    outerLoop
  }

}

