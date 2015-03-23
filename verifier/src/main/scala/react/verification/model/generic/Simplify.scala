package react.verification.model.generic

import dzufferey.smtlib._
import dzufferey.utils._
import dzufferey.utils.LogLevel._
import react.utils.{Qepcad, QepcadPrinter, ArithmeticSimplification}
import Utils._

//the formula
//a set of variable to keep (position, orientation for the frame)
//try to eliminate as much as possible

//TODO equality propagation

class Simplify(robot: GenericRobot) {

  

  protected def findEqualVariables(conjuncts: List[Formula]): List[(Variable, Variable)] = {
    conjuncts.flatMap{
      case Eq(v1 @ Variable(_), v2 @ Variable(_)) =>
        Some(v1 -> v2)
      case Eq(Plus(Times(Literal(-1l)|Literal(-1)|Literal(-1.0), v1 @ Variable(_)), v2 @ Variable(_)),
             (Literal(0) | Literal(0l) | Literal(0.0))) =>
        Some(v1 -> v2)
      case Eq(Plus(v2 @ Variable(_), Times(Literal(-1l)|Literal(-1)|Literal(-1.0), v1 @ Variable(_))),
             (Literal(0) | Literal(0l) | Literal(0.0))) =>
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

  protected class QeSimplifier(f: Formula, bound: Int, useAssumptions: Boolean) {

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
      !isNeeded(v) && relevant(Set(v)).forall(Qepcad.isSupported)
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
      val clauses = clausesFor(vars)
      val fvs = clauses.freeVariables -- vars
      if (fvs.size + vars.size > bound) {
        Logger("Simplify", Info, "too many variable to eliminate " + vars.mkString(",") + " using qepcad: " + fvs.size)
        None
      } else {
        Logger("Simplify", Info, "try to eliminate " + vars.mkString(",") + " using qepcad.")
        Some(Qepcad.query(fvs.toList, vars.toList, clauses, assumptions(fvs)))
      }
    }

    def result: Formula = {
      def tryEliminate(vs: List[Variable]): Formula = vs match {
        case x :: xs =>
          mkQuery(Set(x)) match {
            case Some(q) =>
              try {
                val ir = irrelevant(Set(x))
                val f2 = FormulaUtils.getConjuncts(q.execute(false)) //TODO option for McCallum proj
                new QeSimplifier(And(f2 ::: ir :_*), bound, useAssumptions).result
              } catch {
                case _: Throwable =>
                  tryEliminate(xs)
              }
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
    frameVariables.contains(v) || robot.inputs.exists(_.v == v)
  }

  //TODO try a global slqf query
  def slfqQuery {
    //TODO further normalization
    val allSupported = And(robot.conjuncts.filter(Qepcad.isSupported):_*)
    println(QepcadPrinter.printFormula(allSupported))
  }


  def normalize(useQepcad: Option[Int], safe: Boolean = true) = {

    def query(f: Formula): Formula = {
      val vars = f.freeVariables
      useQepcad match {
        case Some(b) if vars.size <= b && vars.size > 0 =>
          val query = Qepcad.query(vars.toList, Nil, f, None)
          try query.execute(!safe)
          catch { case _: Throwable => f }
        case _ => f
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
      case And(lst @ _*) => And(lst.map(process(_, fct)):_*)
      case Or(lst @ _*) => Or(lst.map(process(_, fct)):_*)
      case other => fct(other)
    }

    def tryQE(f: Formula) = {
      useQepcad match {
        case Some(b) => new QeSimplifier(f, b, false).result
        case None => f
      }
    }

    var constraints = robot.constraints 
    var nFv = constraints.freeVariables.size
    var oFv = nFv + 1
    while (nFv < oFv) {
      oFv = nFv
      val constraints2 = process(constraints, simplify)
      val constraints3 = equalityPropagation(constraints2)
      val constraints4 = process(constraints3, syntacic)
      constraints = tryQE(constraints4)
      nFv = constraints.freeVariables.size
    } 
    fixTypes(constraints)
    val dynamic = robot.dynamic.filter(constraints.freeVariables)
    new GenericRobot(robot.id, robot.pg, robot.bBox, robot.frame, robot.inputs, dynamic, constraints)
  }

}