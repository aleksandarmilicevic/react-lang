package react.utils

import dzufferey.smtlib._
import dzufferey.utils._
import dzufferey.utils.LogLevel._

object DRealQuery {

  def getSolutions(f: Formula, precision: Double, timeout: Long, canScale: Iterable[Variable]): Option[Map[Variable,Double]] = {
    val fname = if (Logger("DRealQuery", Debug)) Some(Namer("query") + ".smt2") else None
    val arg = Array[String]("--in", "--model", "--worklist-fp")
    val solver = new DRealHack(QF_NRA, "dReal", arg, Some(precision), true, false, fname, 1)
    
    val (_normalized, factors) = normalizeRanges(f, canScale)
    val normalized = FormulaUtils.getConjuncts(_normalized)

    normalized.foreach( c => fixTypes(c) )
    normalized.foreach( c => solver.assert(c) )
    //println(normalized.mkString("\n"))

    solver.checkSat(timeout) match {
      case Sat(Some(model)) =>
        //println(model)
        val allVars = f.freeVariables
        val scaledValues = allVars.map( v =>
          try {
            model(v) match {
              case ValD(d) => v -> d
              case ValI(i) => v -> i.toDouble
              case other => sys.error("expected Double, found: " + other)
            }
          } catch { case _: java.util.NoSuchElementException =>
            Logger("GenericRobot", Warning, "initSolution for " + v + " not found ?? using 0.0")
            v -> 0.0
          }
        ).toMap
        val values = scaledValues.map{ case (v,d) => v -> (d * factors.getOrElse(v, 1.0)) }
        Some(values)
      case Sat(None) => sys.error("could not get model!")
      case UnSat => None
      case Unknown => sys.error("unknown !?!")
      case Failure(f) => sys.error("dReal failed: " + f)
    }
  }

  def fixTypes(f: Formula) {
    val t = new FormulaUtils.Traverser {
      override def traverse(f: Formula) {
        super.traverse(f)
        if (f.tpe == Int) f.setType(Real)
      }
    }
    t.traverse(f)
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

