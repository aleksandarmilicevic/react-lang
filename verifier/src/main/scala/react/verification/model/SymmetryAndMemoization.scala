package react.verification.model

import react.message._
import react.verification.ignore
import react.verification.environment._
import react.verification.modelchecker.HashStateStore._ //RichState
import scala.collection.mutable.Map
import scala.collection.concurrent.TrieMap

/** Cache computed motion to increase the efficiency of the modlechecker when the motion is expensive to compute. */
trait SymmetryAndMemoization extends Motion2D {

  /** get the state of what the motion depends on
   *  must inclue t (since motion is not linear)
   *  if rotationSymmetry is set to false then this method must also include _orientation
   */
  def getMotionDepsState(t: Int): Array[Byte]

  /** both translation and rotation symmetry
   *  if this is set to false then getMotionDepsState must include _orientation
   */
  @ignore var rotationSymmetry = true

  ///** how many element to keep in the cache */
  //@ignore var maxCacheSize = 1000000

  /** Returns a identifier of the class of motion (including static parameter)
   *  if this is defined then object with the same motionClass will share the same the same cache
   */
  def motionClass: Option[String] = None

  /** maps dependencies to dx, dy, dθ
   *  if rotationSymmetry then those are given assumming _orientation = 0
   */
  private val cache = motionClass match {
    case Some(id) => SymmetryAndMemoization.getCache(id)
    case None => Map[RichState,(Double,Double,Double)]()
  }

  private def move(dx: Double, dy: Double, da: Double) {
    if (rotationSymmetry) {
      val co = math.cos(orientation)
      val so = math.sin(orientation)
       x += dx * co - dy * so
      y += dx * so + dy * co
      orientation += da
    } else {
      x += dx
      y += dy
      orientation += da
    }
  }

  abstract override protected def moveFor(t: Int) {
    val deps = getMotionDepsState(t)
    if (cache contains deps) {
      val (dx,dy,da) = cache(deps)
      move(dx,dy,da)
    } else {
      val oldX = x
      val oldY = y
      val oldO = orientation
      x = 0
      y = 0
      if (rotationSymmetry) orientation = 0
      super.moveFor(t)
      val dx = x - oldX
      val dy = y - oldY
      val da = orientation - oldO
      x = oldX
      y = oldY
      orientation = oldO
      //add to the cache
      cache += ((deps: RichState) -> (dx,dy,da))
      //move (for real this time)
      move(dx, dy, da)
    }
  }

}

object SymmetryAndMemoization {
  
  protected val lock = new java.util.concurrent.locks.ReentrantLock()

  protected var cache = Map[String,TrieMap[RichState,(Double,Double,Double)]]()

  def getCache(ident: String): TrieMap[RichState,(Double,Double,Double)] = {
    lock.lock
    try {
      if (cache contains ident) cache(ident)
      else {
        val c = new TrieMap[RichState,(Double,Double,Double)]()
        cache += (ident -> c)
        c
      }
    } finally {
      lock.unlock
    }
  }
}
