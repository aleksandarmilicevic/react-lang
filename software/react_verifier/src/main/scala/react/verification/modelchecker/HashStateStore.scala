package react.verification.modelchecker

import org.apache.commons.codec.binary.Hex

import scala.math.Ordering.Implicits
import java.lang.Comparable

object HashStateStore {

  implicit class RichState(val state: Array[Byte]) /*extends AnyVal*/ extends Comparable[RichState] {

    def compareWithArray(array: Array[Byte]): Int = {
      val sizeDiff = state.size compareTo array.size
      if (sizeDiff == 0) {
        var i = 0
        while (i < state.size) {
          val cntDiff = state(i) compareTo array(i)
          if (cntDiff != 0) {
            return cntDiff
          }
          i += 1
        }
        0
      } else {
        sizeDiff
      }
    }
    
    def compareTo(rs: RichState): Int = compareWithArray(rs.state)

    override def toString = Hex.encodeHexString(state)

    override def equals(obj: Any) = obj match {
      case array: Array[Byte] => compareWithArray(array) == 0
      case s: RichState => compareWithArray(s.state) == 0
      case _ => false
    }

    override def hashCode = {
      var hash = 0
      var i = 0
      while (i < state.size) {
        hash = hash ^ (Integer.rotateLeft(state(i).hashCode, i))
        i += 1
      }
      hash
    }

  }

}

import HashStateStore._

class HashStateStore extends scala.collection.mutable.HashSet[RichState] {
}
