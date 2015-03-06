package react.verification.model.generic

import dzufferey.smtlib._
import react.utils.QepcadPrinter

//the formula
//a set of variable to keep (position, orientation for the frame)
//try to eliminate as much as possible

class Simplify(robot: GenericRobot) {

  //constraints that involves only the given variables
  def extendedAssumptions(vs: Set[Variable]) = {
    //(robot.quaternionRanges ++ robot.conjuncts).filter(_.freeVariables.forall(vs contains _))
    robot.conjuncts.filter(_.freeVariables.forall(vs contains _)).filter(QepcadPrinter.isSupported(_))
  }

  def relevant(vs: Set[Variable]): List[Formula] = {
    robot.conjuncts.filter(_.freeVariables.exists(vs contains _))
  }

  val frameVariables = Set(
    robot.frame.x, robot.frame.y, robot.frame.z,
    robot.frame.a, robot.frame.i, robot.frame.j, robot.frame.k
  )

  def canEliminate(v: Variable): Boolean = {
    !frameVariables.contains(v) &&
    relevant(Set(v)).forall(QepcadPrinter.isSupported)
  }

  def findCandidates: List[Variable] = {
    robot.dynamic.filter(canEliminate)
  }

  def clausesFor(vars: Set[Variable]) = {
    And(relevant(vars):_*)
  }

  def eliminationQuery = {
    val cs = findCandidates
    mkQuery( cs.toSet)
  }

  def splitQueries = {
    val cs = findCandidates
    cs.foreach(c => {
      mkQuery(Set(c))
      println("")
      println("")
      println("")
    })
  }

  def mkQuery(vars: Set[Variable]) {
    val clauses = clausesFor(vars)
    val fvs = clauses.freeVariables -- vars
    val assumptions = {
      val a = extendedAssumptions(fvs)
      if (a.isEmpty) None else Some(And(a: _*))
    }
    QepcadPrinter.query(fvs.toList, vars.toList, clauses, assumptions).print
  }

  //TODO try a global slqf query

  def slfqQuery {
    //TODO further normalization
    val allSupported = And(robot.conjuncts.filter(QepcadPrinter.isSupported):_*)
    println(QepcadPrinter.printFormula(allSupported))
  }

}
