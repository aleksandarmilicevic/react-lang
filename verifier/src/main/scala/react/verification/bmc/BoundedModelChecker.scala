package react.verification.bmc

import react._
import react.verification._
import react.verification.ghost._
import react.utils._
import react.verification.modelchecker._
import dzufferey.smtlib._

import dzufferey.utils.LogLevel._
import dzufferey.utils.Logger

//TODO XXX FIXME: for the moment this is an incomplete hack to try parameter synthesis!

//The idea:
// 1. execute the controller for nbrSteps and record the outputs
// 2. unfold the equations over the recorded controller output and the goals/safety constraints
// 3. send to the solver and wait for an answer

class BoundedModelChecker(world: WorldProxy, nbrSteps: Int) {

  import ModelChecker._
  
  
  def getControllerTraces: (List[Int], Seq[Trace]) = {
    var d = 0
    var t = List(0)
    def step(prefix: Trace, time: Int, depth: Int): Seq[Trace] = {
      if (depth > nbrSteps) {
        Seq(prefix)
      } else {
        val s = prefix.stop
        world.restoreState(s)
        val dt = world.timeToNext.toInt
        if (d < depth) {
          d += 1
          t ::= dt
        }
        val oneMoreStep = world.controllerStep(dt, prefix, 0, 1)
        val traces = oneMoreStep.flatMap( t => step(t, time + dt, depth + 1) )
        traces.toSeq
      }
    }
    val initState = world.saveStateCompact
    val prefix = Trace(initState)
    val traces = step(prefix, 0, 0)
    (t.reverse, traces)
  }

  def getStateEquations(traces: Seq[Trace]): Formula = {
    assert(!traces.isEmpty)
    assert(world.world.models.forall(_.hasEquations))
    def oneState(idx: Int, s: State): Formula = {
      world.restoreState(s)
      val eqs = world.world.models.map(_.stateEquations(idx))
      And(eqs:_*)
    }
    def mkEqs(idx: Int, traces: Seq[Trace]): List[Formula] = {
      if (traces.head.length > 0) {
        val (heads, tails) = traces.map(_.step).unzip
        Or( heads.map(_._1).map(oneState(idx, _)):_* ) :: mkEqs(idx + 1, tails)
      } else {
        val heads = traces.map(_.start)
        Or( heads.map(oneState(idx, _)):_* ) :: Nil
      }
    }
    //TODO should we skip 0 ?
    val init = traces //.map(_.step._2)
    And( mkEqs(0, init):_* )
  }

  def getEvolutionEquations(depth: Int): Formula = {
    assert(world.world.models.forall(_.hasEquations))
    val eqs = (0 until depth).flatMap( i => world.world.models.map(_.stateEquations(i)) )
    And( eqs:_* )
  }

  //TODO how do we add the goal and the properties

  def getEquations(depth: Int): Formula = {
    ???
  }

}
