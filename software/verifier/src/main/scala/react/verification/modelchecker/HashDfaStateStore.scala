package react.verification.modelchecker

import react.utils._
import react.verification.StateStore
import HashStateStore._
import net.automatalib.words.{Word, WordBuilder}
import net.automatalib.util.automata.fsa.DFAs
import net.automatalib.util.automata.Automata
import net.automatalib.automata.fsa.impl.compact.CompactDFA
import net.automatalib.words.impl.Alphabets

class HashDfaStateStore(hashTblSize: Int = 100000000) {

  assert(hashTblSize >= 0)

  protected val dfa = new StateStore()

  protected val hash = new HashStateStore()

  protected var hashCnt = 0

  def add(state: Array[Byte]) {
    hash += state
    hashCnt += 1
    if (hashCnt > hashTblSize) {
      hashCnt = hash.size
      if (hashCnt > hashTblSize) {
        compact
      }
    }
  }
  
  def add(states: Iterable[Array[Byte]]) {
    states foreach add
  }

  def contains(state: Array[Byte]) = {
    hash.contains(state) ||
    dfa.contains(StateStore.stateToWord(state))
  }

  def clear {
    hash.clear
    dfa.clear
  }
  
  protected def compact {
    Logger("HashDfaStateStore", LogNotice, "compacting state store")
    def union(a: CompactDFA[Integer], b: CompactDFA[Integer]) = {
      val d = DFAs.or(a, b, dfa.alphabet)
      //println("d1: " + d.size)
      StateStore.inPlaceMinimize(d) //TODO intermediate minimization here ?
      //println("d2: " + d.size)
      d
    }
    val v1 = hash.view.par
    val v2 = v1.map( s => dfa.dfaFromWord(StateStore.stateToWord(s.state)) )
    val newDfa = v2.reduce( union )
    dfa.addDFA(newDfa)
    dfa.minimize()
    Logger("HashDfaStateStore", LogInfo, "dfa size: " + dfa.size)
    hash.clear()
    hashCnt = 0
  }

}
