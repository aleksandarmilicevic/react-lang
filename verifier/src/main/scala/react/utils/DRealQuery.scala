package react.utils

import dzufferey.smtlib._
import dzufferey.utils._
import dzufferey.utils.LogLevel._

object DRealQuery {
  
  def getSolutions(f: Formula, precision: Double, timeout: Long): Option[Map[Variable,Double]] = {

    val cmd = "dReal"
    //val cmd = "dReal3"
    //val cmd = "/home/zufferey/work/projects/dreal3_dz/bin/dReal"
    //val cmd = "/home/zufferey/work/projects/dreal4_ch/bin/dReal"

    //val arg = Array[String]("--in", "--model")
    val arg = Array[String]("--in", "--model", "--worklist-fp", "--ncbt")

    val fname = if (Logger("DRealQuery", Debug)) Some(Namer("query") + ".smt2") else None
    val solver = new DRealHack(QF_NRA, cmd, arg, Some(precision), true, false, fname, 1)
    
    val conj = {
      val f2 = FormulaUtils.map({
          case Literal(i: Int) => Literal(i.toDouble)
          case Literal(l: Long) => Literal(l.toDouble)
          case other => other
        }, f)
      FormulaUtils.getConjuncts(f2)
    }
    conj.foreach( c => fixTypes(c) )
    conj.foreach( c => solver.assert(c) )

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
        Some(scaledValues)
      case Sat(None) => sys.error("could not get model!")
      case UnSat => None
      case Unknown => sys.error("unknown !?!")
      case Failure(f) => sys.error("dReal failed: " + f)
    }
  }

  def getSolutions(f: Formula, precision: Double, timeout: Long, canScale: Iterable[Variable]): Option[Map[Variable,Double]] = {
    val (normalized, factors) = normalizeRanges(f, canScale)
    getSolutions(normalized, precision, timeout).map( unscaleRange(_, factors) )
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

  def unscaleRange(model: Map[Variable, Double], factors: Map[Variable, Double]) = {
    model.map{ case (v,d) => v -> (d * factors.getOrElse(v, 1.0)) }
  }

  def multiplyRange(f: Formula, factors: Map[Variable, Double]) = {
    FormulaUtils.map({
      case v @ Variable(_) if factors.getOrElse(v, 1.0) != 1.0 => Times(Literal(factors(v)), v)
      case other => other
    }, f)
  }

  def getRangeFactor(f: Formula, vs: Iterable[Variable], range: Double = 2.0): Map[Variable,Double] = {
    val bounds = parseBounds(f, vs)
    bounds.flatMap{ case (v,(lb,ub)) => 
      if (lb != Double.NegativeInfinity && ub != Double.PositiveInfinity) {
        Some( v -> ( (ub - lb) / range ) )
      } else {
        None
      }
    }
  }

  def normalizeRanges(f: Formula, vs: Iterable[Variable], range: Double = 2.0): (Formula, Map[Variable,Double]) = {
    val factors = getRangeFactor(f, vs, range)
    val replaced = multiplyRange(f, factors)
    (replaced, factors)
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

