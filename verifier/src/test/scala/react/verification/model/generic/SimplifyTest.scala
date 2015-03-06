package react.verification.model.generic

import org.scalatest._
import dzufferey.smtlib._
import dzufferey.utils.Logger

class SimplifyTest extends FunSuite {

  Logger.disallow("Typer")

  test("qepcad queries") {
    //val robot = GenericRobot(Resources.playground, Resources.path + "seg_eqns_simple.txt")
    //val robot = GenericRobot(Resources.playground, Resources.path + "seg_eqns_preprocessed.txt")
    val robot = GenericRobot(Resources.playground, Resources.path + "seg_eqns_trig.txt")
    val simplifier = new Simplify(robot)
    //simplifier.eliminationQuery
    simplifier.splitQueries
  }

  test("slqf query") {
    //val robot = GenericRobot(Resources.playground, Resources.path + "seg_eqns_simple.txt")
    val robot = GenericRobot(Resources.playground, Resources.path + "seg_eqns_preprocessed.txt")
    val simplifier = new Simplify(robot)
    //simplifier.eliminationQuery
    simplifier.slfqQuery
  }

  /* usgin Z3 (weaker but faster)
  from z3 import *
  b,c,x = Reals('b c x')
  f = Exists(x, b*x+c==0);
  tac = Tactic('qe')
  tac = With(tac, qe_nonlinear=True)
  print tac.param_descrs() 
  f0 = Simplify(f);
  print Simplify(tac(f0).as_expr());
  */

}
