package react.verification.environment

import math._

/** An immobile obstacle in the world (2D → z = ±∞)
 *  (x,y) denotes the lower left corner */
class Box2D(val x: Double,
            val y: Double,
            val orientation: Double,
            val width: Double,
            val depth: Double) {

  override def toString = {
    "Box2D(" + x + ", " + y + ", " + orientation + ", " + width + ", " + depth + ")"
  }

  def writeAsSVG(writer: java.io.BufferedWriter, color: String = "rgb(0, 0, 0)") = {
    val stroke = math.min(0.1, math.min(depth/10,width/10))
    writer.write("<rect x=\""+x+"\" y=\""+y+"\" width=\""+width+"\" height=\""+depth+"\" transform=\"rotate("+math.toDegrees(orientation)+" "+ x +" "+ y + ")\" stroke-width=\""+stroke+"\" stroke=\""+color+"\" />")
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

  val cornersX: Array[Double] = {
    val wx = width * cos(orientation)
    val dx = -depth * sin(orientation)
    Array(
      x,
      x + wx,
      x + dx,
      x + wx + dx
    )
  }
  val cornersY: Array[Double] = {
    val wy = width * sin(orientation)
    val dy = depth * cos(orientation)
    Array(
      y,
      y + wy,
      y + dy,
      y + wy + dy
    )
  }

//val corners: Array[(Double, Double)] = {
//  val wx = width * cos(orientation)
//  val wy = width * sin(orientation)
//  val dx = -depth * sin(orientation)
//  val dy = depth * cos(orientation)
//  Array(
//    (x, y),
//    (x + wx, y + wy),
//    (x + dx, y + dy),
//    (x + wx + dx, y + wy + dy)
//  )
//}

  def inThisFrame(x: Double, y: Double): (Double, Double) = {
    val dx = x - this.x
    val dy = y - this.y
    //val a = atan2(dy, dx) - orientation
    val dx2 = dx * cos(-orientation) - dy * sin(-orientation)
    val dy2 = dx * sin(-orientation) + dy * cos(-orientation)
    (dx2, dy2)
  }

  def inGlobalFrame(x: Double, y: Double): (Double, Double) = {
    val dx = this.x + x * cos(orientation) - y * sin(orientation)
    val dy = this.y + x * sin(orientation) + y * cos(orientation)
    (dx, dy)
  }

  def contains(x: Double, y: Double, error: Double = 1e-6): Boolean = {
    val (dx2, dy2) = inThisFrame(x, y)
    dx2 >= -error && dx2 <= width + error && dy2 >= -error && dy2 <= depth + error
  }
  def contains(p: (Double, Double)): Boolean = {
    contains(p._1, p._2)
  }

  def distance(x: Double, y: Double): Double = {
    //transform the sensor location in the box frame
    val (x2, y2) = inThisFrame(x, y)
    //http://stackoverflow.com/questions/5254838/calculating-distance-between-a-point-and-a-rectangular-box-nearest-point
    val dx = max(max(-x2, 0), x2 -width)
    val dy = max(max(-y2, 0), y2 -depth)
    sqrt(dx*dx + dy*dy);
  }

  def center = {
  //inGlobalFrame(width/2, depth/2)
    var x4 = cornersX(0) + cornersX(1) + cornersX(2) + cornersX(3)
    var y4 = cornersY(0) + cornersY(1) + cornersY(2) + cornersY(3)
    (x4 / 4.0, y4 / 4.0)
  }
  
  protected def radius = {
    sqrt(width*width + depth*depth) / 2
  }

  //TODO this is wrong! (fix when have more time)
  def collides(b: Box2D): Boolean = {
    def c(a: Box2D, b: Box2D) = {
      a.contains(b.cornersX(0), b.cornersY(0)) ||
      a.contains(b.cornersX(1), b.cornersY(1)) ||
      a.contains(b.cornersX(2), b.cornersY(2)) ||
      a.contains(b.cornersX(3), b.cornersY(3)) ||
      a.contains(b.center)
      //b.corners.exists( p => a.contains(p._1, p._2) ) || a.contains(b.center)
    }
    c(this, b) || c(b, this)
  }

  /** for each side, returns (a,b,c) such that ax + by + c = 0 */
  def cartesianEqs: List[(Double,Double,Double)] = {
    def mkEq(x: Double, y: Double, dx: Double, dy: Double) = (-dy, dx, - dx*y + dy*x)
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
  //  println("  a = " + a + ", b = " + b + ", c = " + c)
      // a * (p.x + k*d.x) + b * (p.y + k*d.y) + c = 0
      // k = (-c -a*p.x -b*p.y) / (a*d.x + b*d.y)
      val num = -c  - a * point._1  - b * point._2
      val den = a * direction._1 + b * direction._2
  //  println("  num = " + num + ", den = " + den)
      //look for the degenrated case and ignore them
      if (den >= -error && den <= error) {
  //    println("  XXX")
        None
      } else {
        val k = num/den
        val ix = point._1 + k * direction._1
        val iy = point._2 + k * direction._2
  //    println("  k = " + k + ", x = " + ix + ", iy = " + iy)
        if ( contains(ix, iy, error) ) {
  //      println("  k = " + k)
          Some(k)
        } else {
  //      println("  ---")
          None
        }
      }
    }
  }

  def toMillimeters = new Box2D(x * 1000, y * 1000, orientation, width * 1000, depth * 1000)
  
}
