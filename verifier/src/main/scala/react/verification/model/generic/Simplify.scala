package react.verification.model.generic

import dzufferey.smtlib._
import dzufferey.utils._
import dzufferey.utils.LogLevel._
import react.utils.{Qepcad, QepcadPrinter, ArithmeticSimplification, DRealQuery, IntegerLit}
import Utils._

//the formula
//a set of variable to keep (position, orientation for the frame)
//try to eliminate as much as possible

//TODO equality propagation

class Simplify(robot: GenericRobot) {

  var qepcadSafeProjection = true
  var qepcadTimeout = Qepcad.defaultTimeout
  var qepcadBound = 6

  //TODO generalize: instead of x = y, we can do x = f(\vec y)
  protected def findEqualVariables(conjuncts: List[Formula]): List[(Variable, Variable)] = {
    conjuncts.flatMap{
      case Eq(v1 @ Variable(_), v2 @ Variable(_)) =>
        Some(v1 -> v2)
      case Eq(Plus(Times(IntegerLit(-1), v1 @ Variable(_)), v2 @ Variable(_)), IntegerLit(0)) =>
        Some(v1 -> v2)
      case Eq(Plus(v2 @ Variable(_), Times(IntegerLit(-1), v1 @ Variable(_))), IntegerLit(0)) =>
        Some(v1 -> v2)
      case _ =>
        None
    }
  }

  def equalityPropagation(f: Formula): Formula = {
    val conjuncts = FormulaUtils.getConjuncts(f)
    val eqs = findEqualVariables(conjuncts).flatMap{
      case (v1, v2) if !isNeeded(v1) => Some(v1->v2)
      case (v1, v2) if !isNeeded(v2) => Some(v2->v1)
      case _ => None
    }.toMap
    Logger("Simplify", Info, "replacing: " + eqs.map{ case (k, v) => k+"->"+v }.mkString(", "))
    f.alpha(eqs)
  }

  protected class QeSimplifier(f: Formula, useAssumptions: Boolean) {

    val conjuncts = FormulaUtils.getConjuncts(f)

    //constraints that involves only the given variables
    def assumptions(vs: Set[Variable]) = {
      if (useAssumptions) {
        val as = conjuncts.filter(_.freeVariables.forall(vs contains _)).filter(Qepcad.isSupported(_))
        if (as.isEmpty) None else Some(And(as:_*))
      } else None
    }
  
    def relevant(vs: Set[Variable]): List[Formula] = partition(vs)._1
    def irrelevant(vs: Set[Variable]): List[Formula] = partition(vs)._2

    def partition(vs: Set[Variable]) = {
      conjuncts.partition(_.freeVariables.exists(vs contains _))
    }

    def canEliminate(v: Variable): Boolean = {
      def check(f: Formula): Boolean = f match {
        case Application(And | Or | Not | Eq | Leq | Geq | Lt | Gt | Plus | Times | Divides | Minus | DRealDecl.pow, args) =>
          args.forall(check)
        case a @ Application(_, _) => !(a.freeVariables contains v)
        case Binding(_, _, f) => check(f)
        case Variable(_) => true
        case Literal(_) => true
      }
      !isNeeded(v) && relevant(Set(v)).forall(check)
    }

    def findCandidates: Set[Variable] = {
      f.freeVariables.filter(canEliminate)
    }

    def clausesFor(vars: Set[Variable]) = {
      And(relevant(vars):_*)
    }

    def eliminationQuery = {
      val cs = findCandidates
      mkQuery( cs.toSet)
    }

    def mkQuery(vars: Set[Variable]) = {
      val clauses0 = clausesFor(vars)
      val (clauses1, unabstract) = ArithmeticSimplification.abstractFormula(clauses0, ArithmeticSimplification.isIntegerPolynomial)
      //sanity check
      val nfvs = clauses1.freeVariables -- clauses0.freeVariables
      assert(nfvs.forall( v => unabstract(v).freeVariables.intersect(vars).isEmpty ))

      val fvs = clauses1.freeVariables -- vars
      if (fvs.size + vars.size > qepcadBound) {
        Logger("Simplify", Info, "too many variable to eliminate " + vars.mkString(",") + " using qepcad: " + fvs.size)
        None
      } else {
        Logger("Simplify", Info, "try to eliminate " + vars.mkString(",") + " using qepcad.")
        val query = Qepcad.query(fvs.toList, vars.toList, clauses1, assumptions(fvs))
        try {
          val clauses2 = query.execute(!qepcadSafeProjection,
                                       Qepcad.defaultMemory,
                                       Qepcad.defaultPrime,
                                       qepcadTimeout)
          val clauses3 = unabstract(clauses2)
          val f2 = FormulaUtils.getConjuncts(clauses3)
          val ir = irrelevant(vars)
          Some( And(f2 ::: ir :_*) )
        } catch {
          case t: Throwable =>
            Logger("Simplify", Info, "failed to eliminate " + t)
            //TODO log
            None
        }
      }
    }

    def result: Formula = {
      def tryEliminate(vs: List[Variable]): Formula = vs match {
        case x :: xs =>
          mkQuery(Set(x)) match {
            case Some(f2) => new QeSimplifier(f2, useAssumptions).result
            case None => tryEliminate(xs)
          }
        case Nil => f
      }
      val cnd = findCandidates.toList
      tryEliminate(cnd)
    }

  }


  val frameVariables = Set(
    robot.frame.x, robot.frame.y, robot.frame.z,
    robot.frame.a, robot.frame.i, robot.frame.j, robot.frame.k
  )

  def isNeeded(v: Variable): Boolean = {
    frameVariables.contains(v) ||
    robot.inputs.exists(_.v == v) //||
    //!robot.dynamic.contains(v)
  }

  def normalize() = {

    def query(f: Formula): Formula = {
      val vars = f.freeVariables
      if (vars.size <= qepcadBound && vars.size > 0) {
          val query = Qepcad.query(vars.toList, Nil, f, None)
          try query.execute(!qepcadSafeProjection,
                            Qepcad.defaultMemory,
                            Qepcad.defaultPrime,
                            qepcadTimeout)
      } else {
        f
      }
    }

    def syntacic(f: Formula): Formula = {
      val f2 = ArithmeticSimplification.pushDerivativesDown(robot.timeVar, robot.dynamic.toSet, f)
      ArithmeticSimplification.polynomialNF(f2)
    }

    def simplify(f: Formula): Formula = {
      val f3 = syntacic(f)
      val (f4, unabstract) = ArithmeticSimplification.abstractFormula(f3, ArithmeticSimplification.isIntegerPolynomial)
      val f5 = query(f4)
      unabstract(f5)
    }

    def process(f: Formula, fct: Formula => Formula): Formula = f match {
      case And(lst @ _*) =>
        val lst2 = lst.par.map(process(_, fct)).seq
        And(lst2:_*)
      case Or(lst @ _*) =>
        val lst2 = lst.par.map(process(_, fct)).seq
        Or(lst2:_*)
      case other => fct(other)
    }

    def tryQE(f: Formula) = new QeSimplifier(f, false).result

    var constraints = robot.constraints 
    var nFv = constraints.freeVariables.size
    var oFv = nFv + 1
    while (nFv < oFv) {
      oFv = nFv
      val constraints2 = constraints //process(constraints, simplify)
      val constraints3 = equalityPropagation(constraints2)
      val constraints4 = process(constraints3, syntacic)
      constraints = tryQE(constraints4)
      nFv = constraints.freeVariables.size
    } 
    DRealQuery.fixTypes(constraints)
    val dynamic = robot.dynamic.filter(constraints.freeVariables)
    new GenericRobot(robot.id, robot.pg, robot.bBox, robot.frame, robot.inputs, dynamic, constraints)
  }

}
