package react.verification.bmc

import react._
import react.verification._
import react.verification.ghost._
import react.utils._
import react.verification.modelchecker._
import react.verification.model.generic.Frame
import react.verification.environment.Box2D
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
          Logger("BoundedModelChecker", Debug, "step " + depth + ", dt = " + dt)
          d += 1
          t ::= dt
        }
        val oneMoreStep = world.controllerStep(dt, prefix, 0, 1)
        val traces = oneMoreStep.flatMap( t => step(t, time + dt, depth + 1) )
        traces.toSeq
      }
    }
    val initState = world.saveState
    val prefix = Trace(initState)
    val traces = step(prefix, 0, 0)
    val tr = t.reverse
    Logger("BoundedModelChecker", Info, "#trace = " + traces.length + ", dts = " + tr.mkString(", "))
    (tr, traces)
  }

  def getStateEquations(traces: Seq[Trace]): Formula = {
    assert(!traces.isEmpty)
    def oneState(idx: Int, s: State): Formula = {
      world.restoreState(s)
      val eqs = world.world.models.map(_.stateEquations(idx))
      Logger("BoundedModelChecker", Info, "equation for state at depth " + idx + "\n  " + eqs.mkString("\n  "))
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

  def getEvolutionEquations(times: Seq[Int]): Formula = {
    val eqs = (0 until nbrSteps).flatMap( i => world.world.models.map(_.unrollEquations(i, times(i))) )
    And( eqs:_* )
  }

  def inBox(x: Variable, y: Variable, _b: Box2D): Formula = {
    val b = _b.toMillimeters
    assert(b.orientation == 0.0, "TODO orientation")
    And(Geq(x, Literal(b.x)),
        Geq(y, Literal(b.y)),
        Leq(x, Literal(b.x + b.width)),
        Leq(y, Literal(b.y + b.depth)))
  }

  def outsideBox(x: Variable, y: Variable, b: Box2D): Formula = Not(inBox(x,y,b))

  def outsideBoxNoDisj(x: Variable, y: Variable, _b: Box2D) = {
    val b = _b.toMillimeters
    val centerX = b.x + b.width/2
    val centerY = b.y + b.depth/2
    if (b.width == b.depth) {
      val r2 = b.width * b.width / 2
      val dx = Minus(x, Literal(centerX))
      val dy = Minus(y, Literal(centerY))
      Gt(Plus(Times(dx,dx),Times(dy,dy)), Literal(r2))
    } else {
      assert(false, "TODO ellipse equations")
    }
  }

  def disjointBoxes(frame: Frame, _bBox: Box2D, _obstacle: Box2D) = {
    val bBox = _bBox.toMillimeters
    val obstacle = _obstacle.toMillimeters
    ???
  }
  
  def disjointBoxesNoDisj(frame: Frame, _bBox: Box2D, _obstacle: Box2D) = {
    val bBox = _bBox.toMillimeters
    val obstacle = _obstacle.toMillimeters
    val offsetX = bBox.x + bBox.width / 2
    val offsetY = bBox.y + bBox.depth / 2
    assert(offsetX == 0.0 && offsetY == 0.0, "TODO equations with offsets: " + bBox + ", " + offsetX + ", " + offsetY)
    val centerFrameX = frame.x
    val centerFrameY = frame.y
    val centerObstacleX = Literal(obstacle.x + obstacle.width/2)
    val centerObstacleY = Literal(obstacle.y + obstacle.depth/2)

    val dx = Minus(centerFrameX, centerObstacleX)
    val dy = Minus(centerFrameY, centerObstacleY)
    val d = Plus(Times(dx,dx),Times(dy,dy))

    assert(bBox.width == bBox.depth && obstacle.width == obstacle.depth, "TODO equations with ellipse")
    val r =  obstacle.width + bBox.width //this is radius * sqrt(2)
    val r2 = Literal(r * r / 2)

    Gt(d, r2)
  }

  def avoidObstacles(frame: Frame, bBox: Box2D, disj: Boolean): Formula = {
    val boxes = world.world.envBoxes
    val oustside = if (disj) boxes.map( disjointBoxes(frame, bBox, _) )
                   else boxes.map( disjointBoxesNoDisj(frame, bBox, _) )
    And(oustside:_*)
  }
  
  //no disjunction generate a sufficient condition that does not contains dijsunctions
  def avoidObstacles(disj: Boolean): Formula = {
    val models = world.world.models
    val eqns = models.flatMap( m => {
      val eqn = m.frames.map{ case (f,b) => avoidObstacles(f, b, disj )}
      //TODO is the indexing right
      (1 to nbrSteps).flatMap( i => {
        val vAt = m.variablesAt(i)
        eqn.map(_.alpha(vAt))
      })
    })
    Logger("BoundedModelChecker", Info, "avoidObstacles:\n  " + eqns.mkString("\n  "))
    And(eqns:_*)
  }

  def goalEquations: Formula = {
    assert(world.world.models.size == 1, "TODO goal for more than one robot")
    val m = world.world.models.head
    val fs = m.frames
    if (fs.size > 1) {
      Logger("BoundedModelChecker", Warning, "using the first frame for the goal")
    }
    val (f,b) = fs.head
    val offsetX = b.x + b.width / 2
    val offsetY = b.y + b.depth / 2
    assert(offsetX == 0.0 && offsetY == 0.0, "TODO goal equations with offsets")
    val goals = world.world.targets.map{ case (i, b) =>
      val vAt = m.variablesAt(i)
      inBox(f.x, f.y, b).alpha(vAt)
    }
    Logger("BoundedModelChecker", Info, "goals:\n  " + goals.mkString("\n  "))
    And(goals:_*)
  }

  def getEquations: Formula = {
    val (times, traces) = getControllerTraces
    val states = getStateEquations(traces)
    val evolution = getEvolutionEquations(times)
    val obstacles = avoidObstacles(false)
    val goals = goalEquations
    FormulaUtils.simplifyBool(And(states, evolution, obstacles, goals))
  }
  
  //inputs and parameters should not be scaled, all the rest is scaled
  def getVariablesToUnScale(f: Formula) = {
    val toKeep = world.world.models.flatMap( m => {
      val init = m.parameters.toSet
      val inputs = m.controlInputs
      (1 until nbrSteps).foldLeft(init)( (acc, i) => {
        val mAt = m.variablesAt(i)
        acc ++ inputs.map(_.alpha(mAt))
      })
    })
    toKeep.toSet
  }

  def getVariablesToScale(f: Formula) = {
    f.freeVariables -- getVariablesToUnScale(f)
  }
  
  //TODO drawing a picture

  //val unscaleFactor = 1.0
  //val unscaleFactor = 0.001
  val unscaleFactor = 0.000001
  val scaleRange = 2
  //val scaleRange = 200
  //val scaleRange = 0.001

  def run {
    try {
      Logger.disallow("Typer")
      Logger("BoundedModelChecker", Notice, world.stateSpaceDescription)
      Logger("BoundedModelChecker", Notice, world.schedulerToString)
      val cstr = getEquations
      val varsU = getVariablesToUnScale(cstr)
      val varsS = getVariablesToScale(cstr)
      val factors = DRealQuery.getRangeFactor(cstr, varsS, scaleRange) ++ varsU.map( _ -> unscaleFactor ).toMap
      val cstrScaled = DRealQuery.multiplyRange(cstr, factors)
      val startTime = java.lang.System.currentTimeMillis()
      DRealQuery.getSolutions(cstrScaled, 1e-3, 1800 * 1000) match {
        case Some(scaledValues) =>
          val values = DRealQuery.unscaleRange(scaledValues, factors)
          val sorted = values.map(_.toString).toSeq.sorted
          println("Solution:\n  " + sorted.mkString("\n  "))
        case None =>
          Logger("BoundedModelChecker", Error, "no solutions!!")
      }
      val endTime = java.lang.System.currentTimeMillis()
      Logger("BoundedModelChecker", Notice, "dReal took: " + (endTime - startTime))
    } catch {
      case e: Exception =>
        Logger("BoundedModelChecker", Error, e.toString)
        e.printStackTrace
    }
  }

}
