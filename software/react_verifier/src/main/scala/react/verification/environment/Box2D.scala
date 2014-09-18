package react.verification.environment

import math._

/** An immobile obstacle in the world (2D → z = ±∞) */
class Box2D(val x: Double,
            val y: Double,
            val orientation: Double,
            val width: Double,
            val depth: Double) {

  override def toString = {
    "Box2D(" + x + ", " + y + ", " + orientation + ", " + width + ", " + depth + ")"
  }

  override def equals(obj: Any) = {
    if (obj.isInstanceOf[Box2D]) {
      //TODO account for rounding error ??
      val b = obj.asInstanceOf[Box2D]
      x == b.x &&
      y == b.y &&
      orientation == b.orientation &&
      width == b.width &&
      depth == b.depth
    } else {
      false
    }
  }

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

  def contains(x: Double, y: Double, error: Double = 1e-6) = {
    val dx = x - this.x
    val dy = y - this.y
    //val a = atan2(dy, dx) - orientation
    val dx2 = dx * cos(-orientation) - dy * sin(-orientation)
    val dy2 = dx * sin(-orientation) + dy * cos(-orientation)
    dx2 >= -error && dx2 <= width + error && dy2 >= -error && dy2 <= depth + error
  }

  def collides(b: Box2D): Boolean = {
    b.corners exists ( p => contains(p._1, p._2) )
  }

  /** for each side, returns (a,b,c) such that ax + by + c = 0 */
  def cartesianEqs: List[(Double,Double,Double)] = {
    def mkEq(x: Double, y: Double, dx: Double, dy: Double) = (-dy, dx, dx*y - dy*x)
    val wx = width * cos(orientation)
    val wy = width * sin(orientation)
    val dx = -depth * sin(orientation)
    val dy = depth * cos(orientation)
    List(
      mkEq(x, y, wx, wy),
      mkEq(x, y, dx, dy),
      mkEq(x + wx + dx, y + wy + dy, wx, wy),
      mkEq(x + wx + dx, y + wy + dy, dx, dy)
    )
  }

  /** compute the intersections of this cube with a line */
  def intersectLine(point: (Double,Double), direction: (Double, Double), error: Double = 1e-6): List[Double] = {
    //println("intersecting " + toString + " with ")
    //println("  x = " + point._1 + " + " + direction._1 + " * k ")
    //println("  y = " + point._2 + " + " + direction._2 + " * k ")
    cartesianEqs.flatMap{ case (a,b,c) =>
      // a * (p.x + k*d.x) + b * (p.y + k*d.y) + c = 0
      // k = (-c -a*p.x -b*p.y) / (a*d.x + b*d.y)
      val num = -c  - a * point._1  - b * point._2
      val den = a * direction._1 + b * direction._2
      //look for the degenrated case and ignore them
      if (den >= -error && den <= -error) {
        None
      } else {
        val k = num/den
        val ix = point._1 + k * direction._1
        val iy = point._2 + k * direction._2
        if ( contains(ix, iy, error) ) {
          //println("  k = " + k)
          Some(k)
        } else {
          //println("  ---")
          None
        }
      }
    }
  }
  
}
