package react.utils

object Ratio {

  def apply(num: Long, denom: Long) = new Ratio(num, denom)
 
  def gcd(i : Long, j : Long) : Long = {
    def gcd1(i : Long, j : Long) : Long = {
      if (j == 0) i
      else gcd1(j, i % j)
    }
    if (i.abs == 0 && j.abs == 0) 1 //TODO better
    else if (i.abs == 0) j.abs
    else if (j.abs == 0) i.abs
    else gcd1(i.abs, j.abs)
  }

  val zero = new Ratio(0,1)
  val one = new Ratio(1,1)
  val mone = new Ratio(-1,1)


  def max(a: Ratio, b: Ratio) = if (a > b) a else b

}

object RatioImplicits {
  implicit def i2r(i: Int): Ratio = new Ratio(i, 1l)
  implicit def l2r(l: Long): Ratio = new Ratio(l, 1l)
}

//TODO those operations are prone to overflow
class Ratio(_num: Long, _denom: Long) extends Ordered[Ratio] {
  def this(n: Long) = this(n, 1)
  def this(n: Int) = this(n, 1)
  def this() = this(0, 1)

  val (n, d) = {
    val gcd = Ratio.gcd(_num.abs, _denom.abs)
    val sign = if (_num < 0l ^ _denom < 0l) -1 else 1
    val _num2 = sign * _num.abs / gcd
    val _denom2 = _denom.abs / gcd
    assert(_num * _denom2 == _num2 * _denom)
    (_num2, _denom2)
  }

  def num = n
  def denom = d
  def toDouble = num.toDouble / denom.toDouble
  def abs = if (num < 0) new Ratio(-num, denom) else this
  def *(r : Ratio) = new Ratio(num * r.num, denom * r.denom)
  def +(r : Ratio) = new Ratio(num * r.denom + r.num * denom, denom * r.denom)
  def -(r : Ratio) = new Ratio(num * r.denom - r.num * denom, denom * r.denom)
  def /(r : Ratio) = new Ratio(num * r.denom, denom * r.num)
  def %(r : Ratio) = new Ratio( (num * r.denom) % (denom * r.num), denom * r.denom)

  def *(l : Long) = new Ratio(num * l, denom)
  def +(l : Long) = new Ratio(num + l * denom, denom)
  def -(l : Long) = new Ratio(num - l * denom, denom)
  def /(l : Long) = new Ratio(num, denom * l)
  def %(l : Long) = new Ratio( num % (denom * l), denom)

  override def <(r : Ratio) : Boolean = num * r.denom < r.num * denom
  override def <=(r : Ratio) : Boolean = num * r.denom <= r.num * denom
  override def >(r : Ratio) : Boolean = num * r.denom > r.num * denom
  override def >=(r : Ratio) : Boolean = num * r.denom >= r.num * denom
  def ==(r : Ratio) : Boolean = num * r.denom == r.num * denom
  def !=(r : Ratio) : Boolean = num * r.denom != r.num * denom
  override def compare(r : Ratio) : Int =  (num * r.denom) compare (r.num * denom)
  override def toString = num + "/" + denom

  def isWhole = denom == 1l
  def toLong = { assert(isWhole); num }
}

