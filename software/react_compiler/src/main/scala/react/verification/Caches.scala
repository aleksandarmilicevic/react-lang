package react.verification

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.ConcurrentHashMap
import scala.reflect.ClassTag

//TODO make size increase on the fly

/** A class to store/restore _immutable_ values of an arbitrary type */
class Cache[T: ClassTag](nbrByte: Int) {
  assert(nbrByte > 0 && nbrByte <= 4)

  val maxNbrElt = 1 << (8*nbrByte)

  protected val cnt = new AtomicInteger()
  protected val map = new ConcurrentHashMap[T,Int]()
  protected val arr = Array.ofDim[T](maxNbrElt)

  protected def mkIdx(idx: Int) =
    if (idx < 0) idx + 2*maxNbrElt
    else idx

  def idx(value: T): Int = {
    if (map containsKey value) {
      map get value
    } else {
      val j = cnt.getAndIncrement
      val i = map.putIfAbsent(value, j)
      val index = if (i != 0) i else j
      arr(index) = value
      index
    }
  }

  def value(idx: Int): T = {
    arr(mkIdx(idx))
  }

  def returnSome: T = {
    assert(cnt.get > 0)
    do {
      val i = util.Random.nextInt % cnt.get
      val v = arr(i)
      if (v != null) {
        return v
      }
    } while(true)
    sys.error("unreachable")
  }

}

object SymbolCache extends Cache[Symbol](1) {
}

object OrientationCache extends Cache[react.robot.Orientation](1) {
  import react.robot._
  idx(North)
  idx(South)
  idx(East)
  idx(West)
}

