package react.verification

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.ConcurrentHashMap
import scala.reflect.ClassTag
import scala.reflect.runtime.universe.TypeTag

//TODO make size increase on the fly

/** A class to store/restore _immutable_ values of an arbitrary type */
class Cache[T: ClassTag] {

  val chunkSize = 256
  val chunks = Array.ofDim[Array[T]](256)
  val maxNbrElt = 256 * 256

  protected val cnt = new AtomicInteger()
  protected val map = new ConcurrentHashMap[T,Int]()
  protected val lck = new java.util.concurrent.locks.ReentrantLock()

  private def read(idx: Int): T = {
    assert(idx < maxNbrElt)
    val a1 = chunks(idx >> 8)
    if (a1 == null) sys.error("Cache: element at " + idx + " not found")
    else a1(idx & 0xff)
  }

  private def write(idx: Int, elt: T) {
    assert(idx < maxNbrElt)
    val i1 = idx >> 8
    val i2 = idx & 0xff
    val a1 = chunks(idx >> 8)
    if (a1 == null) {
      lck.lock
      try {
        val a = Array.ofDim[T](256)
        chunks(i1) = a
        a(i2) = elt
      } finally {
        lck.unlock
      }
    } else {
      a1(i2) = elt
    }
  }

  def idx(value: T): Int = {
    if (map containsKey value) {
      map get value
    } else {
      val j = cnt.getAndIncrement
      val i = map.putIfAbsent(value, j)
      val index = if (i != 0) i else j
      write(index, value)
      index
    }
  }

  def value(idx: Int): T = read(idx)

  def returnSome: T = {
    assert(cnt.get > 0)
    do {
      val i = util.Random.nextInt % cnt.get
      val v = read(i)
      if (v != null) {
        return v
      }
    } while(true)
    sys.error("unreachable")
  }

  def size = map.size

}

object SymbolCache extends Cache[Symbol] {
}

object OrientationCache extends Cache[react.robot.Orientation] {
  import react.robot._
  idx(North)
  idx(South)
  idx(East)
  idx(West)
}

object MetaCache {
  private val map = new ConcurrentHashMap[String,Any]()

  private def getCache[T: ClassTag](id: String): Cache[T] = {
    if (map containsKey id) {
      (map get id).asInstanceOf[Cache[T]]
    } else {
      val c = new Cache[T]
      val cOld = map.putIfAbsent(id, c).asInstanceOf[Cache[T]]
      if (cOld != null) cOld else c
    }
  }

  def getCache[T : TypeTag : ClassTag]: Cache[T] = {
    val id = implicitly[TypeTag[T]].tpe.toString
    getCache[T](id)
  }

}
