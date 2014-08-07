package react.verification

import react._

import net.automatalib.util.automata.fsa.DFAs
import net.automatalib.util.automata.Automata
import net.automatalib.automata.fsa.impl.compact.CompactDFA
import net.automatalib.words.impl.Alphabets
import net.automatalib.words.{Word, WordBuilder}
import scala.collection.mutable.HashSet
import java.nio.ByteBuffer
//import java.util.concurrent.ConcurrentHashMap
//import java.util.Collections

class ModelChecker[R <: Robot](
    world: World,
    robots: Array[R],
    ghosts: Array[GhostAgent],
    safety: Array[SafetyProperty]) {

  type State = Array[Byte]

  val rLength = robots.length
  val gLength = robots.length
  val statePartition = {
    val arr = Array[Int](rLength + gLength)
    for (i <- 0 until rLength) arr(i) = robots(i).length(world)
    for (i <- 0 until gLength) arr(i+rLength) = ghosts(i).length(world)
    arr
  }
  val totalLength = statePartition.foldLeft(0)(_ + _)

  def getCurrentState: State = {
    val buffer = ByteBuffer.allocate(totalLength) 
    for(r <- robots) r.serialize(world, buffer)
    for(g <- ghosts) g.serialize(world, buffer)
    buffer.array
  }

  def restoreSate(s: State) {
    val buffer = ByteBuffer.wrap(s) 
    for(r <- robots) r.deserilize(world, buffer)
    for(g <- ghosts) g.deserilize(world, buffer)
  }

  //most compact representation of the state: automaton
  var permanentStates = new StateStore()

  def addToPermanent(s: Seq[State]) {
    if (!s.isEmpty) {
      def union(a: CompactDFA[Integer], b: CompactDFA[Integer]) = {
        DFAs.or(a, b, permanentStates.alphabet)
      }
      val s2 = s.par.map(permanentStates.stateToDFA)
      val dfa = s2.reduce( union )
      permanentStates.addDFA(dfa)
      permanentStates.minimize()
    }
  }

  //to represent the transient states between the round
  val transientStates = HashSet[State]()
  //val transientStates = Collections.newSetFromMap(new ConcurrentHashMap<State, Boolean>())

  val frontier = new java.util.ArrayDeque[State]()

  def innerLoop(s: State): Seq[State] = {
    //...
    sys.error("TODO")
  }
  
  def outerLoop = {
    //...
    sys.error("TODO")
  }

  def verify = {

  }

}

