package react.utils

import dzufferey.smtlib._

object IntegerLit {
  def unapply(f: Formula): Option[Int] = f match {
    case Literal(i: Int) => Some(i)
    case Literal(l: Long) if l.isValidInt => Some(l.toInt)
    case Literal(f: Float) if f.isValidInt => Some(f.toInt)
    case Literal(d: Double) if d.isValidInt => Some(d.toInt)
    case _ => None
  }
  def apply(i: Int) = Literal(i).setType(Int)
}

object LongIntLit{
  def unapply(f: Formula): Option[Long] = f match {
    case Literal(i: Int) => Some(i.toLong)
    case Literal(l: Long) => Some(l.toInt)
    case Literal(f: Float) if f.isWhole => Some(f.toLong)
    case Literal(d: Double) if d.isWhole => Some(d.toLong)
    case _ => None
  }
  def apply(i: Long) = Literal(i).setType(Int)
}

object FloatLit {
  def unapply(f: Formula): Option[Double] = f match {
    case Literal(i: Int) => Some(i.toDouble)
    case Literal(l: Long) => Some(l.toDouble)
    case Literal(f: Float) => Some(f.toDouble)
    case Literal(d: Double) => Some(d)
    case _ => None
  }
  def apply(d: Double) = Literal(d).setType(Real)
  def apply(f: Float)  = Literal(f.toDouble).setType(Real)
  def apply(i: Int)    = Literal(i.toDouble).setType(Real)
  def apply(l: Long)   = Literal(l.toDouble).setType(Real)
}

