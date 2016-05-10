package react.utils

import dzufferey.smtlib._
import dzufferey.utils._
import dzufferey.utils.LogLevel._

object ArithmeticSimplification {

  //TODO this is not really arithmetic but algebra
  //TODO bug in the derivative of the trig ?
  def pushDerivativesDown(dt: Variable, dynamic: Set[Variable], f: Formula): Formula = f match {
    case Application(DRealDecl.timeDerivative, List(expr)) if expr.freeVariables.forall(v  => !(dynamic contains v) && v != dt) =>
      Literal(0.0)
    case Application(DRealDecl.timeDerivative, List(v)) if v == dt =>
      Literal(1.0)
    case Application(DRealDecl.timeDerivative, List(Plus(args @ _*))) =>
      Plus(args.map(a => pushDerivativesDown(dt, dynamic, DRealDecl.timeDerivative(a))):_*)
    case Application(DRealDecl.timeDerivative, List(Minus(args @ _*))) =>
      Minus(args.map(a => pushDerivativesDown(dt, dynamic, DRealDecl.timeDerivative(a))):_*)
    case Application(DRealDecl.timeDerivative, List(Times(args @ _*))) =>
      val n = args.length
      val parts = (0 until n).map( i => {
        val prefix = args.take(i)
        val d = pushDerivativesDown(dt, dynamic, DRealDecl.timeDerivative(args(i)))
        val suffix = args.drop(i+1)
        val together = (prefix :+ d) ++ suffix
        Times(together:_*)
      })
      Plus(parts:_*)
    case Application(DRealDecl.timeDerivative, List(Divides(f, g))) =>
      val p1 = Times(pushDerivativesDown(dt, dynamic, DRealDecl.timeDerivative(f)), g)
      val p2 = Times(f, pushDerivativesDown(dt, dynamic, DRealDecl.timeDerivative(g)))
      val p3 = DRealDecl.pow(g, Literal(2.0))
      Divides(Minus(p1, p2), p3)
    case Application(DRealDecl.timeDerivative, List(Application(DRealDecl.sin, args))) =>
      assert(args.length == 1)
      val dArgs = pushDerivativesDown(dt, dynamic, DRealDecl.timeDerivative(args(0)))
      Times(DRealDecl.cos(args:_*), dArgs)
    case Application(DRealDecl.timeDerivative, List(Application(DRealDecl.cos, args))) =>
      assert(args.length == 1)
      val dArgs = pushDerivativesDown(dt, dynamic, DRealDecl.timeDerivative(args(0)))
      Times(Literal(-1.0), DRealDecl.sin(args:_*), dArgs)
    case Application(DRealDecl.timeDerivative, List(Application(DRealDecl.pow, List(expr, Literal(n: Double))))) =>
      if (n == 0.0) Literal(0.0)
      else Times(Literal(n), DRealDecl.pow(expr, Literal(n-1)), pushDerivativesDown(dt, dynamic, DRealDecl.timeDerivative(expr)))
    case Application(DRealDecl.timeDerivative, List(Application(DRealDecl.pow, List(expr, Literal(n: Long))))) =>
      if (n == 0l) Literal(0)
      else Times(Literal(n), DRealDecl.pow(expr, Literal(n-1)), pushDerivativesDown(dt, dynamic, DRealDecl.timeDerivative(expr)))
    case a @ Application(fct, args) => fct(args.map(pushDerivativesDown(dt, dynamic, _)):_*).setType(a.tpe)
    case b @ Binding(bt, vs, f) => Binding(bt, vs, pushDerivativesDown(dt, dynamic, f)).setType(b.tpe)
    case other => other
  }



  protected class PolySimp(f: Formula) {
    //assert(f.tpe == Int || f.tpe == Real, "f.tpe = " + f.tpe + ", f = " + f)
    assert(f.boundVariables.isEmpty)

    val vars = f.freeVariables.toSeq
    val idx = vars.zipWithIndex.toMap
    def i2v(i: Int) = vars(i)

    //TODO should we generalize exponents to rational instead of long ?

    protected case class Monomial(coeff: Ratio, exponents: IndexedSeq[Long]) {

      def *(v: Variable) = {
        val i = idx(v)
        Monomial(coeff,  exponents.updated(i, exponents(i) + 1))
      }
      
      def /(v: Variable) = {
        val i = idx(v)
        Monomial(coeff,  exponents.updated(i, exponents(i) - 1))
      }

      def *(c: Long) = {
        if (c == 0l) {
          Monomial(Ratio.zero, exponents.map( _ => 0l ))
        } else {
          Monomial(coeff * c, exponents)
        }
      }
      
      def *(m: Monomial) = {
        if (coeff == Ratio.zero || m.coeff == Ratio.zero) {
          Monomial(Ratio.zero, exponents.map( _ => 0l ))
        } else {
          Monomial( coeff * m.coeff,
                    for (i <- exponents.indices) yield exponents(i) + m.exponents(i) )
        }
      }
      
      def +(m: Monomial) = {
        assert(canAdd(m))
        Monomial(coeff + m.coeff, exponents)
      }

      def canAdd(m: Monomial) = {
        exponents.indices.forall(i => exponents(i) == m.exponents(i) )
      }

      def toFormula: Formula = {
        val c = if (coeff.isWhole) Literal(coeff.toLong)
                else Divides(Literal(coeff.num), Literal(coeff.denom))
        if (coeff == Ratio.zero || exponents.forall( _ == 0l )) {
          c
        } else {
          val prod = exponents.zipWithIndex.flatMap{ case (e, i) =>
              if (e == 0l) None
              else if (e == 1l) Some(i2v(i))
              else Some(DRealDecl.pow(i2v(i), FloatLit(e)))
            }
          val elts = if (coeff == Ratio.one && !prod.isEmpty) prod
                     else c +: prod
          if (elts.size == 1) elts.head
          else Times(elts:_*)
        }
      }

    }

    protected case class Polynomial(ms: Seq[Monomial]) {
  
      def reduce = {
        def process(acc: Seq[Monomial], rest: Seq[Monomial]): Seq[Monomial] = {
          if (rest.isEmpty) {
            acc
          } else if (rest.head == zero) {
            process(acc, rest.tail)
          } else if (rest.size == 1) {
            rest ++ acc
          } else {
            val hd = rest.head
            val (same, tail) = rest.tail.partition(_.canAdd(hd))
            val sum = same.foldLeft(hd)(_ + _)
            process(sum +: acc, tail)
          }
        }
        val ms2 = process(Seq(), ms)
        val ms3 = if (ms2.isEmpty) Seq(zero) else ms2
        Polynomial(ms3)
      }

      def +(m: Monomial): Polynomial = {
        Polynomial(m +: ms).reduce
      }

      def *(m: Monomial): Polynomial = {
        Polynomial(ms.map(_ * m)).reduce
      }

      def +(p: Polynomial): Polynomial = {
        ms.foldLeft(p)( _ + _ )
      }

      def *(p: Polynomial): Polynomial = {
        val ps = ms.map( m => p * m )
        ps.foldLeft(Polynomial(Seq()))( _ + _ )
      }

      def toFormula: Formula = {
        if (ms.isEmpty) Literal(0l)
        else {
          val mf = ms.map(_.toFormula)
          if (mf.size == 1) mf.head
          else Plus(mf:_*)
        }
      }

    }

    protected val zero = Monomial(Ratio.zero, vars.map( _ => 0l).toIndexedSeq)
    protected val one = Monomial(Ratio.one, vars.map( _ => 0l).toIndexedSeq)
    protected val minusOne = Monomial(Ratio.mone, vars.map( _ => 0l).toIndexedSeq)

    protected def constant(l: Long): Polynomial = {
      Polynomial(Seq(Monomial(new Ratio(l,1l), zero.exponents)))
    }
    
    protected def ratio(n: Long, d: Long): Polynomial = {
      Polynomial(Seq(Monomial(new Ratio(n,d), zero.exponents)))
    }

    def mkPolynomial(f: Formula): Polynomial = f match {
      case Minus(a, b) => mkPolynomial(a) + (mkPolynomial(b) * minusOne)
      case Plus(lst @ _*) => lst.foldLeft(Polynomial(Seq()))( _ + mkPolynomial(_) )
      case Times(lst @ _*) => lst.foldLeft(constant(1))( _ * mkPolynomial(_) )
      case Application(DRealDecl.pow, List(e, Literal(l))) =>
        val p = mkPolynomial(e)
        var c = l match {
          case l: Long => l
          case d: Double if d.isWhole => d.toLong
          case other => sys.error("mkPolynomial, not supported: exponent = " + other)
        }
        assert(c >= 0, "TODO negative exponent not yet implemented")
        var acc = Polynomial(Seq(one))
        while(c > 0l) {
          c-= 1
          acc *= p
        }
        acc
      case LongIntLit(l) => constant(l) 
      case Divides(LongIntLit(n), LongIntLit(d)) => ratio(n, d)
      case v @ Variable(_) => Polynomial(Seq(one * v))
      case other => sys.error("mkPolynomial, not supported: " + other.toStringFull)
    }

    def processEq(f: Formula): Formula = f match {
      case Application(fct @ (Eq | Leq | Lt | Geq | Gt), List(a, b)) =>
        val pa = mkPolynomial(a)
        Logger("ArithmeticSimplification", Debug, " a: " +  a)
        Logger("ArithmeticSimplification", Debug, "pa: " + pa)
        val pb = mkPolynomial(b)
        Logger("ArithmeticSimplification", Debug, " b: " +  b)
        Logger("ArithmeticSimplification", Debug, "pb: " + pb)
        val lhs = pa + (pb * minusOne)
        Logger("ArithmeticSimplification", Debug, "lhs: " + lhs)
        Logger("ArithmeticSimplification", Debug, "lhs: " + lhs.toFormula)
        val result = fct(lhs.toFormula, Literal(0l))
        Logger("ArithmeticSimplification", Info, "original: " + f)
        Logger("ArithmeticSimplification", Info, "result:   " + result)
        result
        //TODO factor the gcd of coeffs in lhs
      case Not(ap) => Not(processEq(f))
      case And(lst @ _*) => And(lst.map(processEq):_*)
      case Or(lst @ _*) => Or(lst.map(processEq):_*)
      case other => mkPolynomial(other).toFormula
    }

    def result = processEq(f)

  }

  /**
   * @param f the formula to abstract
   * @param valid a function to test which subparts are valid (not abstracted)
   * @return (f2, fct) where f2 is an abstracted formula and fct a function to turn f2 (or a formula derived from f2) back into f
   */
  def abstractFormula(f: Formula, valid: Formula => Boolean): (Formula, Formula => Formula) = {

    var cnt = 0
    var replacing = Map[Formula, Variable]()
    var replaced = Map[Variable, Formula]()

    def unabstract(f: Formula): Formula = {
      FormulaUtils.map({
        case v @ Variable(_) if replaced contains v => replaced(v)
        case other => other
      }, f)
    }

    def addExpr(_a: Formula): Variable = {
      val a = unabstract(_a)
      if (replacing contains a) {
        replacing(a)
      } else {
        val v = Variable("_abstract_dummy"+cnt).setType(a.tpe)
        cnt += 1
        replaced += (v -> a)
        replacing += (a -> v)
        v
      }
    }

    def check(part: Formula): Formula = {
      if (valid(part)) {
        Logger("ArithmeticSimplification", Debug, "keeping: " + part)
        part
      } else {
        Logger("ArithmeticSimplification", Debug, "abstracting: " + part)
        addExpr(part)
      }
    }

    val f2 = FormulaUtils.map(check, f)

    (f2, unabstract)
  }

  def isIntegerPolynomial(f: Formula): Boolean = {
    def isInteger(f: Formula) = f match {
      case Literal(_: Int) | Literal(_: Long) => true
      case Literal(d: Double) => d.isWhole
      case _ => false
    }
    def check(f: Formula): Boolean = f match {
      case Divides(l1 @ Literal(_), l2 @ Literal(_)) => isInteger(l1) && isInteger(l2)
      case Application((Eq | Leq | Lt | Geq | Gt), List(_, _)) => true
      case Variable(_) | Plus(_*) | Minus(_*) | Times(_*) => true
      case Application(DRealDecl.pow, List(f, i)) => check(f) && !isInteger(f) && isInteger(i)
      case And(_*) | Or(_*) | Not(_) => true
      case other => isInteger(other)
    }
    check(f)
  }

  def isZero(l: Formula) = l match {
    case LongIntLit(0l) => true
    case _ => false
  }

  def simplifyCst(f: Formula) = {
    FormulaUtils.map({
      case Application(DRealDecl.pow, List(LongIntLit(0l), LongIntLit(0l))) => sys.error("undefined: 0^0")
      case Application(DRealDecl.pow, List(LongIntLit(0l), LongIntLit(e))) => LongIntLit(0l)
      case Application(DRealDecl.pow, List(LongIntLit(l), LongIntLit(0l))) => LongIntLit(1l)
      case Application(DRealDecl.pow, List(LongIntLit(l), LongIntLit(e))) if e > 0 => Literal(math.pow(l, e).toLong) //TODO check overflow
      case Application(DRealDecl.pow, List(Literal(l: Double), Literal(e: Double))) => Literal(math.pow(l, e))
      case Application(DRealDecl.cos, List(LongIntLit(0l))) => LongIntLit(1l)
      case Application(DRealDecl.sin, List(LongIntLit(0l))) => LongIntLit(0l)
      case Divides(l1, l2) if isZero(l1) && !isZero(l2)  => LongIntLit(0l)
      case Times(lst @ _*) if lst.exists(isZero) => LongIntLit(0l)
      case other => other
    }, f)
  }

  def polynomialNF(f: Formula): Formula = {
    val level = Debug
    Logger("ArithmeticSimplification", level, "simplifing: " + f)
    val f1 = simplifyCst(f)
    val (f2, unabstract) = abstractFormula(f1, isIntegerPolynomial)
    val f3 = new PolySimp(f2).result
    val f4 = unabstract(f3)
    Logger("ArithmeticSimplification", level, "result: " + f4)
    f4
  }


}
