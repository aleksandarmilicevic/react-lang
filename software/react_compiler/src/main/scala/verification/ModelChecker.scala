package react.verification

import react._

import net.automatalib.util.automata.fsa.DFAs
import net.automatalib.automata.fsa.impl.compact.CompactDFA
import net.automatalib.words.impl.Alphabets
import net.automatalib.words.{Word, WordBuilder}
import scala.collection.mutable.HashSet
//import java.util.concurrent.ConcurrentHashMap
//import java.util.Collections

class ModelChecker(world: World,
                   robots: Seq[Robot],
                   ghosts: Seq[GhostAgent],
                   safety: Seq[SafetyProperty]) {

  type State = Array[Byte]

  //most compact representation of the state: automaton
  val permanentStates = new CompactDFA(ModelChecker.binaryAlphabet)

  //to represent the transient states between the round
  val transientStates = HashSet[State]()
  //val transientStates = Collections.newSetFromMap(new ConcurrentHashMap<State, Boolean>())

  val frontier: Seq[State] = Seq()

  def innerLoop(s: State): Seq[State] = {
    //...
    sys.error("TODO")
  }
  
  def outerLoop: Seq[State] = {
    //...
    sys.error("TODO")
  }

  def verify = {
  }

}

object ModelChecker {

  val binaryAlphabet = Alphabets.integers(0, 2)

  /** convert a state into a binary word */
  def stateToWord(state: Array[Byte]): Word[Int] = {

    val builder = new WordBuilder[Int]

    def addByte(b: Byte) {
      if ((b & 0x1) != 0)  builder.add(1) else builder.add(0)
      if ((b & 0x2) != 0)  builder.add(1) else builder.add(0)
      if ((b & 0x4) != 0)  builder.add(1) else builder.add(0)
      if ((b & 0x8) != 0)  builder.add(1) else builder.add(0)
      if ((b & 0x10) != 0) builder.add(1) else builder.add(0)
      if ((b & 0x20) != 0) builder.add(1) else builder.add(0)
      if ((b & 0x40) != 0) builder.add(1) else builder.add(0)
      if ((b & 0x80) != 0) builder.add(1) else builder.add(0)
    }

    state foreach addByte

    builder.toWord
  }

  def dfaFromWord(w: Word[Int]) = {
    val dfa = new CompactDFA(binaryAlphabet, w.length + 2)
    var last =
      if (w.isEmpty()) {
        dfa.addInitialState(true) 
      } else {
        dfa.addInitialState(false)
      }
    val err = dfa.addState(false)
    for (idx <- 0 until (w.length -1) ) {
      val sym = w.getSymbol(idx)
      val curr = dfa.addState(false)
      dfa.addTransition(last, sym, curr)
      val wrong = if (sym == 0) 1 else 0
      dfa.addTransition(last, wrong, err)
      last = curr
    }
    val curr = dfa.addState(true)
    dfa.addTransition(last, w.lastSymbol, curr)
    dfa.addTransition(curr, 0, err)
    dfa.addTransition(curr, 1, err)
    dfa
  }

}
