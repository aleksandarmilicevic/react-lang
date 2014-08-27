package react.verification

import react._
import react.verification.ghost._

import net.automatalib.util.automata.fsa.DFAs
import net.automatalib.util.automata.Automata
import net.automatalib.automata.fsa.impl.compact.CompactDFA
import net.automatalib.words.impl.Alphabets
import net.automatalib.words.{Word, WordBuilder}
import scala.collection.mutable.HashSet
import scala.collection.GenIterable
import java.nio.ByteBuffer
//import java.util.concurrent.ConcurrentHashMap
//import java.util.Collections


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
  protected val transientStates = HashSet[State]()

  //val transientStates = Collections.newSetFromMap(new ConcurrentHashMap<State, Boolean>())
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
        saveStateWithScheduler
      }
    }
  }

  /** saturates the systems with ghosts inputs */
  def ghostStep(s: State): Iterable[State] = {
    restoreStateWithScheduler(s)
    val bp = new BranchingPoints(world.ghosts)
    val alt = bp.alternatives
    for(i <- 0 until alt) yield {
      restoreStateWithScheduler(s)
      bp.act(i) //TODO add a timeout for infinite loops 
      world.waitUntilStable
      saveStateWithScheduler
    }
  }

  //the inner loop proceeds into two steps.
  //first, it generates all the reachable states by ghost perturbations (simulates user inputs, etc...)
  //then, we do the periodic controller update
  def innerLoop(s: State): Iterable[State] = {
    //the ghost steps
    transientStates.clear()
    val s2 = addDefaultSchedulerState(s)
    putT(s2)
    while(!frontierT.isEmpty) {
      val s = getT
      val s2 = ghostStep(s)
      val s3 = s2.filterNot(transientStates)
      transientStates ++= s3
      s3 foreach putT
    }
    //the controller step
    transientStates foreach putT
    while(!frontierT.isEmpty) {
      val s = getT
      val s2 = controllerStep(s)
      val s3 = s2.filterNot(transientStates)
      transientStates ++= s3
      s3 foreach putT
    }
    val stateLst = transientStates.toList
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

  def verify = {
    defaultSchedulerState = scheduler.saveState
    period = scheduler.computePeriod
    val initState = saveStateWithScheduler
    permanentStates.addState(initState)
    put(initState)
    outerLoop
  }

}

