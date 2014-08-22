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
  
}
