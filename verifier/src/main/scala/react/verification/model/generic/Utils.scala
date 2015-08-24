package react.verification.model.generic

import dzufferey.smtlib._
import dzufferey.utils._
import dzufferey.utils.LogLevel._

object Utils {

  def fixTypes(f: Formula) {
    val t = new FormulaUtils.Traverser {
      override def traverse(f: Formula) {
        super.traverse(f)
        if (f.tpe == Int) f.setType(Real)
      }
    }
    t.traverse(f)
  }

  def parseFormula(e: SExpr): Formula = e match {
    case SAtom("pi") => Literal(math.Pi)
    case SAtom(str) =>
      Misc.toDouble(str) match {
        case Some(l) => Literal(l)
        case None => Variable(str).setType(Real)
      }
    case SApplication(op, args) =>
      val args2 = args map parseFormula
      val symbol: Symbol = op match {
        case "D" =>
          DRealDecl.timeDerivative
        case _ if InterpretedFct(op).isDefined =>
          //println("known symbol: " + op)
          InterpretedFct(op).get
        case _ if DRealDecl.fcts.exists(_.toString == op) =>
          //println("known symbol: " + op)
          DRealDecl.fcts.find(_.toString == op).get
        case other =>
          Logger("GenericRobot", Debug, "unknown symbol: " + op)
          UnInterpretedFct(other)
      }
      (symbol, args2) match {
        case (Minus, List(Literal(d: Double))) => Literal(-d)
        case (Minus, List(f)) => Times(Literal(-1.0),f)
        case _ => symbol(args2:_*)
      }
    case SNil => 
      Logger.logAndThrow("GenericRobot", Error, "expected expression, not ()")
  }

  def occursInDerivative(v: Variable, f: Formula): Boolean = {
    FormulaUtils.collect(false, (acc: Boolean, f) => f match {
      case Application(DRealDecl.timeDerivative, args) =>
        if (And(args:_*).freeVariables contains v) true else acc
      case _ => acc
    }, f)
  }

  def hasDt(f: Formula) = {
    FormulaUtils.collectSymbols(f).contains(DRealDecl.timeDerivative)
  }

  def weaken(f: Formula, delta: Double): Formula = {
    val d = Literal(delta.abs)
    FormulaUtils.map({
      case Eq(a, b) => And(Leq(a, Plus(b, d)), Leq(b, Plus(a, d)))
      case Leq(a, b) => Leq(a, Plus(b, d))
      case Lt(a, b) => Leq(a, Plus(b, d))
      case Geq(a, b) => Geq(Plus(a, d), b)
      case Gt(a, b) => Gt(Plus(a, d), b)
      case a @ And(_*) => And(FormulaUtils.getConjuncts(a):_*)
      case other => other
    }, f)
  }

  def normalizeRanges(f: Formula, vs: Iterable[Variable], range: Double = 2.0): (Formula, Map[Variable,Double]) = {
    val bounds = parseBounds(f, vs)
    val factor = bounds.flatMap{ case (v,(lb,ub)) => 
      if (lb != Double.NegativeInfinity && ub != Double.PositiveInfinity) {
        Some( v -> ( (ub - lb) / range ) )
      } else {
        None
      }
    }
    val replaced = FormulaUtils.map({
      case v @ Variable(_) if factor contains v => Times(Literal(factor(v)), v)
      case other => other
    }, f)
    //TODO this messes the bounds 
    (replaced, factor)
  }
  
  def parseBounds(f: Formula, vs: Iterable[Variable]): Map[Variable, (Double,Double)] = {
    val vs = f.freeVariables
    val init = vs.map(v => v -> (Double.NegativeInfinity, Double.PositiveInfinity)).toMap
    FormulaUtils.getConjuncts(f).foldLeft(init)( (acc, f) => f match {
      case LowerBound(v, b) =>
        val (l, u) = acc(v)
        acc + (v -> (math.max(l, b), u))
      case UpperBound(v, b) =>
        val (l, u) = acc(v)
        acc + (v -> (l, math.min(u, b)))
      case other =>
        acc
    })
  }

}

object RealLit {
  def unapply(f: Formula): Option[Double] = f match {
    case Literal(d: Double) => Some(d)
    case Literal(d: Float)  => Some(d.toDouble)
    case Literal(d: Long)   => Some(d.toDouble)
    case Literal(d: Int)    => Some(d.toDouble)
    case Literal(d: Short)  => Some(d.toDouble)
    case Literal(d: Byte)   => Some(d.toDouble)
    case _                  => None
  }
}

object LowerBound {
  def unapply(f: Formula): Option[(Variable,Double)] = f match {
    //non-strict var first
    case Geq(   v @ Variable(_), RealLit(d) ) => Some(v -> d)
    case Not(Lt(v @ Variable(_), RealLit(d) )) => Some(v -> d)
    //non-strict var second
    case Leq(   RealLit(d), v @ Variable(_)  ) => Some(v -> d)
    case Not(Gt(RealLit(d), v @ Variable(_) )) => Some(v -> d)
    //strict var fist
    case Gt(     v @ Variable(_), RealLit(d)   ) => Some(v -> d)
    case Not(Leq(v @ Variable(_), RealLit(d)  )) => Some(v -> d)
    //strict var second
    case Lt(     RealLit(d), v @ Variable(_)   ) => Some(v -> d)
    case Not(Geq(RealLit(d), v @ Variable(_)  )) => Some(v -> d)
    case _ => None
  }
}

object UpperBound {
  def unapply(f: Formula): Option[(Variable,Double)] = f match {
    //non-strict
    case Leq(v @ Variable(_), RealLit(d)) => Some(v -> d)
    case Geq(RealLit(d), v @ Variable(_)) => Some(v -> d)
    case Not(Gt(v @ Variable(_), RealLit(d))) => Some(v -> d)
    case Not(Lt(RealLit(d), v @ Variable(_))) => Some(v -> d)
    //strict
    case Lt(v @ Variable(_), RealLit(d)) => Some(v -> d)
    case Gt(RealLit(d), v @ Variable(_)) => Some(v -> d)
    case Not(Leq(RealLit(d), v @ Variable(_))) => Some(v -> d)
    case Not(Geq(v @ Variable(_), RealLit(d))) => Some(v -> d)
    case _ => None
  }
}

