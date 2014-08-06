package react

class Vector2D(
    var x: Double,
    var y: Double
  ) extends VectorLike[Vector2D]
{
  def components: Array[Double] = Array(x, y)
  def fromComponents(c: Array[Double]): Vector2D = new Vector2D(c(0), c(1))
}

class Vector3D(
    var x: Double,
    var y: Double,
    var z: Double
  ) extends VectorLike[Vector3D]
{
  def components: Array[Double] = Array(x, y, z)
  def fromComponents(c: Array[Double]): Vector3D = new Vector3D(c(0), c(1), c(2))
}

class Quaternion(
    var x: Double,
    var y: Double,
    var z: Double,
    var w: Double
  ) extends VectorLike[Quaternion]
{
  def components: Array[Double] = Array(x, y, z, w)
  def fromComponents(c: Array[Double]): Quaternion = new Quaternion(c(0), c(1), c(2), c(3))
}

trait VectorLike[T <: VectorLike[T]] {
  def +(rhs: T): T = fromComponents(components.zip(rhs.components).map{ case (a,b) => a + b })
  def -(rhs: T): T = fromComponents(components.zip(rhs.components).map{ case (a,b) => a - b })
  def *(rhs: Double): T = fromComponents(components.map( _ * rhs ))
  def /(rhs: Double): T = fromComponents(components.map( _ / rhs ))
  def norm: Double = math.sqrt(components.foldLeft(0.0)( (acc, x) => acc + x*x ))
  def normL1: Double = components.foldLeft(0.0)( (acc, x) => acc + x.abs )
  def components: Array[Double]
  def fromComponents(cmpts: Array[Double]): T
}

object VectorLike {
  implicit def *[T <: VectorLike[T]](coeff: Double, vec: T): T = vec * coeff
}

