package react.utils

import dzufferey.smtlib._
import dzufferey.utils._
import dzufferey.utils.LogLevel._


object Scaling {
  
  protected def replace(f: Formula, coeffs: Map[Variable,Formula]): Formula = {
    def r(f: Formula): Formula = f match {
      case v @ Variable(_) => coeffs.getOrElse(v, v)
      case other => other
    }
    val scaled = FormulaUtils.map(r, f)
    ArithmeticSimplification.polynomialNF(scaled)
  }
  
  /** Scale variables to extend or reduce their range.
   * @param f the formula in which variables need scaling 
   * @param coeffs maps variable v to coefficient c
   * @return f where all occurences of v are replaced by c*v if (v,c) is in the coefficient
   */
  def byRationalCoeff(f: Formula, coeffs: Map[Variable,Ratio]): Formula = {
    val c2 = coeffs.map{ case (v,c) =>
      val expr =
        if (c.denom == 1l ) {
          if (c.num == 1l ) v
          else Times(Literal(c.num), v)
        } else {
          if (c.num == 1l ) Divides(v,Literal(c.denom))
          else Times(Divides(Literal(c.num),Literal(c.denom)), v)
        }
      v -> expr
    }
    replace(f, c2)
  }
  
  /** Scale variables to extend or reduce their range.
   * @param f the formula in which variables need scaling 
   * @param coeffs maps variable v to coefficient c
   * @return f where all occurences of v are replaced by c*v if (v,c) is in the coefficient
   */
  def byCoeff(f: Formula, coeffs: Map[Variable,Long]): Formula = {
    val c2 = coeffs.map{ case (v,c) => v -> Times(Literal(c), v) }
    replace(f, c2)
  }

  /** Scale variables to get a particular domain.
   * @param f the formula in which variables need scaling 
   * @param domains maps variable v to the size of their domain d
   * @return f where all occurences of v are replaced by (o/d)*v if (v,d) is in the domains and o is the size of the old domain
   */
  def byTarget(f: Formula, domains: Map[Variable,Long]): Formula = {
    //find bounds
    import scala.collection.mutable.{Map => MMap}
    val lbs = MMap[Variable,Long]()
    val ubs = MMap[Variable,Long]()
    def insert(m: MMap[Variable,Long], compare: (Long, Long) => Long, v: Variable, d: Long) {
      if (m contains v) m += (v -> compare(m(v), d))
      else              m += (v -> d)
    }
    def insertL(v: Variable, d: Long) = insert(lbs, math.max, v, d)
    def insertU(v: Variable, d: Long) = insert(ubs, math.min, v, d)
    FormulaUtils.getConjuncts(f).foreach{
      case Leq(v @ Variable(_), Literal(d: Double)) if d.isWhole && domains.contains(v) => insertU(v, d.toLong)
      case Geq(v @ Variable(_), Literal(d: Double)) if d.isWhole && domains.contains(v) => insertL(v, d.toLong)
      case Lt( v @ Variable(_), Literal(d: Double)) if d.isWhole && domains.contains(v) => insertU(v, d.toLong)
      case Gt( v @ Variable(_), Literal(d: Double)) if d.isWhole && domains.contains(v) => insertL(v, d.toLong)
      case Leq(v @ Variable(_), Literal(l: Long))   if domains.contains(v) => insertU(v, l)
      case Geq(v @ Variable(_), Literal(l: Long))   if domains.contains(v) => insertL(v, l)
      case Lt( v @ Variable(_), Literal(l: Long))   if domains.contains(v) => insertU(v, l)
      case Gt( v @ Variable(_), Literal(l: Long))   if domains.contains(v) => insertL(v, l)
      case _ => ()
    }
    //scale
    val coeffs = domains.flatMap{ case (v,d) => 
      if (lbs.contains(v) && ubs.contains(v)) {
        val old = ubs(v) - lbs(v)
        Some(v -> Ratio(old, d))
      } else {
        Logger("ArithmeticSimplification", Warning, "did not find lower or upper bound for " + v)
        None
      }
    }
    byRationalCoeff(f, coeffs)
  }

}
