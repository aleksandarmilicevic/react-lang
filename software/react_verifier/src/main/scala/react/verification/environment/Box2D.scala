package react.verification.environment

import math._

/** An immobile obstacle in the world (2D → z = ±∞) */
class Box2D(val x: Double,
            val y: Double,
            val orientation: Double,
            val width: Double,
            val depth: Double) {

  def corners: Array[(Double, Double)] = {
    val wx = width * cos(orientation)
    val wy = width * sin(orientation)
    val dx = -depth * sin(orientation)
    val dy = depth * cos(orientation)
    Array(
      (x, y),
      (x + wx, y + wy),
      (x + dx, y + dy),
      (x + wx + dx, y + wy + dy)
    )
  }

  def contains(x: Double, y: Double) = {
    val dx = x - this.x
    val dy = y - this.y
    //val a = atan2(dy, dx) - orientation
    val dx2 = dx * cos(-orientation) - dy * sin(-orientation)
    val dy2 = dx * sin(-orientation) + dy * cos(-orientation)
    dx2 >= 0 && dx2 <= width && dy2 >=0 && dy2 <= depth
  }

  def collides(b: Box2D): Boolean = {
    b.corners exists ( p => contains(p._1, p._2) )
  }

  /** for each side, returns [a,b,c] such that ax + by + c = 0 */
  def cartesianEqs = {
    val eqs = Array.ofDim[Double](4,3)
    def mkEq(target: Array[Double], x: Double, y: Double, dx: Double, dy: Double) {
      target(0) = -dy
      target(1) = dx
      target(2) = dx*y - dy*x
    }
    val wx = width * cos(orientation)
    val wy = width * sin(orientation)
    val dx = -depth * sin(orientation)
    val dy = depth * cos(orientation)
    mkEq(eqs(0), x, y, wx, wy)
    mkEq(eqs(1), x, y, dx, dy)
    mkEq(eqs(2), x + wx + dx, y + wy + dy, wx, wy)
    mkEq(eqs(3), x + wx + dx, y + wy + dy, dx, dy)
    eqs
  }

  /** compute the intersections of this cube with a line */
  def intersectLine(point: (Double,Double), direction: (Double, Double)): List[Double] = {
    sys.error("TODO")
  }
  
}
