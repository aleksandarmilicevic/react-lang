package react.verification

import react._
import react.verification.ghost._

import net.automatalib.util.automata.fsa.DFAs
import net.automatalib.util.automata.Automata
import net.automatalib.automata.fsa.impl.compact.CompactDFA
import net.automatalib.words.impl.Alphabets
import net.automatalib.words.{Word, WordBuilder}
import scala.collection.mutable.HashSet
import scala.collection.GenSeq
import java.nio.ByteBuffer
//import java.util.concurrent.ConcurrentHashMap
//import java.util.Collections

//TODO robots and ghosts should be part of the language (compiler project)
//     robots and ghosts should be embedded into the world

//TODO compute the overall period of the system (boundary between the inner and outer loop)

class ModelChecker(world: World) {

  type State = Array[Byte]

  //most compact representation of the state: automaton
  protected var permanentStates = new StateStore()

  def addToPermanent(s: GenSeq[Word[Integer]]) {
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

  def controllerStep(s: State): State = {
    world.restoreState(s)
    sys.error("TODO") //TODO
  }
  def ghostStep(s: State): Seq[State] = {
    world.restoreState(s)
    sys.error("TODO") //TODO
  }

  //the inner loop proceeds into two steps.
  //first, it generates all the reachable states by ghost perturbations (simulates user inputs, etc...)
  //then, we do the periodic controller update
  def innerLoop(s: State): Seq[State] = {
    //the ghost step ...
    transientStates.clear()
    putT(s)
    while(!frontierT.isEmpty) {
      val s = getT
      val s2 = ghostStep(s)
      val s3 = s2.filterNot(transientStates)
      transientStates ++= s3
      s3 foreach putT
    }
    //the update step ...
    val afterGhost = transientStates.toArray
    transientStates.clear()
    for (idx <- afterGhost.indices) {
      val s = afterGhost(idx)
      val p = controllerStep(s)
      afterGhost(idx) = p
    }
    afterGhost
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
    val initState = world.getCurrentState
    permanentStates.addState(initState)
    put(initState)
    outerLoop
  }

}

