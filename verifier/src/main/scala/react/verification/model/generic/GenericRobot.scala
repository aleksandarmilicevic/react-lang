package react.verification.model.generic

import react.message._
import react.runtime.MessageListenerRW
import react.Executor
import react.robot._
import react.utils._
import react.verification.Playground
import react.verification.model._
import react.verification.environment._
import react.verification.modelchecker.BranchingPoint
import dzufferey.smtlib._
import dzufferey.utils._
import dzufferey.utils.LogLevel._
import Utils._

case class Input(v: Variable, topic: String)

case class Frame( x: Variable, y: Variable, z: Variable, //3D vector
                  a: Variable, i: Variable, j: Variable, k: Variable) //quaternion
{
  def description: String = "(frame "+x+" "+y+" "+z+" "+a+" "+i+" "+j+" "+k+")"
}


class GenericRobot( val id: String,
                    val pg: Playground,
                    val bBox: Box2D,
                    val frame: Frame,
                    val inputs: List[Input],
                    val dynamic: List[Variable],
                    val constraints: Formula ) extends GroundRobot(bBox, None) with SymmetryAndMemoization {

  //include t, the inputs (?dynamic? not for the moment, assumed to be transient)
  def getMotionDepsState(t: Int): Array[Byte] = {
    val deps = Array.ofDim[Byte](4 + inputs.length)
    ByteArray.store(deps, 0, t)
    var pos = 4
    for (i <- inputs) {
      ByteArray.store(deps, pos, store(i.v))
      pos += 2
    }
    deps
  }
  override def motionClass = Some("GenericRobot("+inputs.mkString(",")+","+dynamic.mkString(",")+","+constraints)

  protected def mkVar(str: String) = Variable(str).setType(Real)

  val timeVar = mkVar("t")

  def modelDescription: String = {
    val d = UnInterpretedFct("D", Some(Real ~> Real), Nil)
    val variables = constraints.freeVariables ++ dynamic ++ inputs.map(_.v)
    val dt = FormulaUtils.map({
        case Application(DRealDecl.timeDerivative, args) => d(args:_*)
        case other => other
      }, constraints)
    DRealQuery.fixTypes(dt)
    val cjt = FormulaUtils.getConjuncts(dt)

    "~~~ Parameters:\n" +
    variables.toSeq.sortBy( _.name ).mkString("\n") +
    "\n\n" +
    "~~~ Equations:\n" +
    frame.description + "\n" +
    "(bbox "+bBox.x+" "+bBox.y+" "+bBox.orientation+" "+bBox.width+" "+bBox.depth+")\n" +
    inputs.map( i => "(input "+i.v+")\n").mkString +
    dynamic.map( d => "(dynamic "+d+")\n").mkString +
    cjt.map(Printer.toString).mkString("\n")
  }

  def aboutTheEqns {
    Console.println(inputs.size + " inputs")
    Console.println(allVars.size + " state variables")
    Console.println(allVars.filter(occursInDerivative(_, constraints)).size + " variables under a derivative")
    Console.println(conjuncts.size + " clauses")
    val eqs = conjuncts.filter{ case Eq(_,_) => true; case _ => false }
    Console.println(eqs.size + " equalities")
    val ineqs = conjuncts.filter{ case Lt(_,_) | Leq(_,_) | Gt(_,_) | Geq(_,_) => true; case _ => false }
    Console.println(ineqs.size + " inequalities")
    //some more ?
  }

  //indexing variables (to generate code)
  val allVars = (constraints.freeVariables -- inputs.map(_.v)).toSeq
  val v2i = allVars.zipWithIndex.toMap
  val i2v = Array.ofDim[Variable](allVars.size)
  v2i.foreach{ case (v, i) => i2v(i) = v }

  //for the second order derivative...
  //we can define some new variables as y = dx/dt
  //first step is to find the implicit variables.

  val dtSuffix = "_dt"
  def isDt(v: Variable): Boolean = v.name endsWith dtSuffix
  def dtize(v: Variable): Variable = Variable(v.name + dtSuffix).setType(v.tpe)
  def unDt(v: Variable): Variable = {
    if (isDt(v)) Variable(v.name.dropRight(dtSuffix.length)).setType(v.tpe)
    else v
  }
  protected def replaceDt(f: Formula): Formula = {
    FormulaUtils.map({
      case Application(DRealDecl.timeDerivative, List(v @ Variable(_))) => dtize(v)
      case a @ Application(DRealDecl.timeDerivative, _) => sys.error("not normalized: " + a)
      case other => other
    }, f)
  }
  protected def partitionSolution(m: Map[Variable, Double]) = {
    val (mDt,mNormal) = m.partition( p => isDt(p._1))
    (mNormal, mDt.map{ case (v,d) => unDt(v) -> d})
  }

    
  protected def clean(precision: Double, m: Map[Variable, Double]) = m.mapValues( v => if (v.abs <= precision) 0.0 else v )

  def initSolutionKinsol(guess: Map[Variable, Double], guessDt: Map[Variable, Double], precision: Double): (Map[Variable, Double], Map[Variable, Double]) = {
    val in = inputs.flatMap( i => store.get(i.v).map( v => i.v -> Literal(v.toDouble)) ).toMap
    val known = poseValues ++ in
    val knownValues = known.map{ case (k, Literal(d: Double)) => k -> d }
    val guesses = guess ++ guessDt.map{ case (k,v) => dtize(k) -> v }
    val guesses2 = clean(precision, guesses)
    val values = kinsol.solve(knownValues, guesses2)
    val allValues = clean(precision, values ++ knownValues)
    partitionSolution(allValues)
  }

  protected def getKnown = {
    val in = inputs.flatMap( i => store.get(i.v).map( v => i.v -> Literal(v.toDouble)) ).toMap
    in.mapValues{ case Literal(d: Double) if d.abs < 1e-5 => Literal(0.0); case other => other }
    //(poseValues ++ in).mapValues{ case Literal(d: Double) if d.abs < 1e-5 => Literal(0.0); case other => other }
  }

  protected def dRealInitEquations: Formula = {
    val bounds1 = angleRanges ::: ranges
    val cstr1 = conjuncts.map(replaceDt)
    val bounds2 = And(cstr1:_*).freeVariables.toList.filter(_.name.endsWith(dtSuffix)).flatMap( v => {
        List( Lt(v, Literal( 10000)),
              Gt(v, Literal(-10000)))
      })
    val cstr2 = cstr1//.map(replace).map(ArithmeticSimplification.polynomialNF)
    val bounds = bounds1 ::: bounds2//.map(replace)
    val cstr3 = cstr2 //cstr2.flatMap(c => FormulaUtils.getConjuncts(weaken(c, precision/2)))
    val cstr = bounds ::: cstr3
    And(cstr:_*)
  }

  def initSolutionDReal(precision: Double = 0.1): (Map[Variable, Double], Map[Variable, Double]) = {
    //Logger("GenericRobot", Error, "computing initial solution for " + x + "," + y + " " + orientation + " " + store)
    val cstr0 = dRealInitEquations
    val known = getKnown
    def replace(f: Formula) = {
      FormulaUtils.map({ case v @ Variable(_) => known.getOrElse(v,v)
                         case x => x }, f)
    }
    val cstr = replace(cstr0) //?? ArithmeticSimplification.polynomialNF ??
    DRealQuery.getSolutions(cstr, precision, 1000, dynamic) match {
      case Some(values) =>
        val knownValues = known.map{ case (k, Literal(d: Double)) => k -> d }
        val solution = values ++ knownValues
        partitionSolution(solution)
      case None =>
        sys.error("no initial value!")
    }
  }
  
  def initSolution(precision: Double, useKinsol: Boolean): (Map[Variable, Double], Map[Variable, Double]) = {
    val (guess1, guess2) = initSolutionDReal(precision)
    //Logger("GenericRobot", Error, "initSolution " + precision)
    //Logger("GenericRobot", Error, guess1.toString)
    //Logger("GenericRobot", Error, guess2.toString)
    if (useKinsol) {
      val (s1, s2) = initSolutionKinsol(guess1, guess2, precision)
    //Logger("GenericRobot", Error, s1.toString)
    //Logger("GenericRobot", Error, s2.toString)
      (s1, s2)
    } else {
      (guess1, guess2)
    }
  }


  //from 2D pose (x,y,θ) to 3D vector + quaternion
  def poseConstrains = {
    val q = Angle.quaternionFromTheta(orientation)
    List(
      Eq(frame.x, Literal(1000 * x)), //TODO discretization
      Eq(frame.y, Literal(1000 * y)), //TODO discretization
      //z should be constrained by the robot structural equations
      //TODO precision
      Eq(frame.a, Literal(q.x)),
      Eq(frame.i, Literal(q.y)),
      Eq(frame.j, Literal(q.z)),
      Eq(frame.k, Literal(q.w))
    )
  }
  
  def poseValues = {
    val q = Angle.quaternionFromTheta(orientation)
    Map(
      frame.x -> Literal(1000 * x),
      frame.y -> Literal(1000 * y),
      frame.a -> Literal(q.x),
      frame.i -> Literal(q.y),
      frame.j -> Literal(q.z),
      frame.k -> Literal(q.w)
    )
  }


  def quaternionRanges = {
    val quaternionSuffixes = Set(".q_a",".q_i",".q_j",".q_k")
    val qs = dynamic.filter( v => quaternionSuffixes.contains(v.name.takeRight(4)) )
    qs.flatMap( v => List(Leq(v, Literal(1.0)), Leq(Literal(-1.0), v)) )
  }

  def posRanges = {
    dynamic.flatMap( v => {
      if (v.name endsWith ".dx") {
        List( Lt(v, Literal(1000 * (pg.xMax + pg.xDiscretization))),
              Gt(v, Literal(1000 * (pg.xMin - pg.xDiscretization))))
      } else if (v.name endsWith ".dy") {
        List( Lt(v, Literal(1000 * (pg.yMax + pg.yDiscretization))),
              Gt(v, Literal(1000 * (pg.yMin - pg.yDiscretization))))
    //} else if (v.name endsWith "dz") {
    //  List(
    //  )
      } else {
        Nil
      }
    })
  }

  def angleRanges = {
    dynamic.flatMap( v => {
      if (v.name endsWith ".angle") {
        List( Lt(v, Literal(math.Pi)),
              Gt(v, Literal(-math.Pi)) )
      } else {
        Nil
      }
    })
  }

  def ranges = posRanges ::: quaternionRanges

  def conjuncts = FormulaUtils.getConjuncts(constraints)

  val timeIndependentConstraints = {
    conjuncts.filter(c => c.freeVariables.forall(v => !dynamic.contains(v)))
  }

  protected val timeDependentConstraints = {
    conjuncts.filter(c => c.freeVariables.exists(v => dynamic.contains(v)))
  }

  val strucutralTimeDependentConstraints = {
    timeDependentConstraints.filter(!hasDt(_))
  }

  val differentialConstraints = {
    timeDependentConstraints.filter(hasDt)
  }

  //decompose (Or ...) into a small HA: List of (invariant, dynamics) pairs
  def collectModes(f: Formula): List[(Formula, List[Formula])] = {
    val ds = FormulaUtils.getDisjuncts(f)
    ds.map( d => {
      val cs = FormulaUtils.getConjuncts(d)
      val (dt, cond) = cs.partition(hasDt)
      val c = if (cond.size == 0) True()
              else if (cond.size == 1) cond.head
              else And(cond:_*)
      (c, dt)
    })
  }

  def printDRealStub( endTime: Double, maxUnfold: Int): (DRealHack, Map[Variable,Variable]) = {
    val solver = DReal(QF_NRA, 0.1, "test.smt2")
    timeIndependentConstraints.foreach(solver.assert(_))
    //the different modes
    val modes = differentialConstraints.map(collectModes)
    //versionning the variables by jump number
    def alpha(i: Int): Map[Variable, Variable] = {
      dynamic.foldLeft(Map[Variable,Variable]())( (acc, v) => {
        val v1 = if (i == 0) v
                 else if (i == maxUnfold) Variable(v.name + "_final").setType(v.tpe)
                 else Variable(v.name + "_" + i).setType(v.tpe)
        acc + (v -> v1)
      }).toMap
    }
    val connect = UnInterpretedFct("connect", Some(Real ~> Real ~> Bool), Nil)
    def holder(f: Formula) = {
      ???
      Variable("holder_" + ??? ).setType(Real)
    }
    for (i <- 0 until maxUnfold) {
      for ( part <- modes;
            (guard, dyn) <- part ) {
        if (guard == True()) {
          dyn.foreach( d => {
            val v = Variable(Namer("flow")).setType(Bool)
            if(i == 0) solver.declareODE(v.name, d)
            solver.assert(connect(holder(d), v))
          })
        } else {
          //TODO the jump condition 
          //TODO how variables changes at jump (x_n = x_{n-1})
          //TODO connecting the flow to holder
          ???
        }
      }
    }
    //time
    val timeVars = (0 until maxUnfold).map( i => Variable(timeVar.name + "_" + i).setType(Real) )
    solver.assert(Eq(Plus(timeVars:_*), Literal(endTime)))
    timeVars.foreach(v => solver.assert(Leq(Literal(0.0), v)))
    timeVars.foreach(v => solver.assert(Leq(v, Literal(endTime))))
    //TODO integrating the holders between each jump and time variables
    ???
    //TODO should be done for all the version of the variables!
    for (i <- 0 until maxUnfold) {
      strucutralTimeDependentConstraints.foreach(solver.assertForallT(1, Literal(0.0), Literal(endTime), _))
    }
    //
    (solver, ???) //solver still needs an objective
  }

  val inK = (inputs.map(_.v) ++ Seq(frame.x, frame.y, frame.a, frame.i, frame.j, frame.k)).toIndexedSeq
  lazy val kinsol = new KINSOL(inK, replaceDt(constraints))

  val inV = inputs.map(_.v)
  lazy val ida = new IDA(inV, constraints)

  override def finalize {
    ida.clean
    kinsol.clean
  }

  override protected def moveFor(t: Int) = {
    try {
      //val tolerance = 1e-14
      val tolerance = 1e-3
      //Logger("GenericRobot", Error, this.toString)
      val (init, initDt) = initSolution(tolerance, false)
      if (!initDt.isEmpty) {
        x = (init(frame.x) + initDt.getOrElse(frame.x, 0.0) * t / 1000) / 1000
        y = (init(frame.y) + initDt.getOrElse(frame.y, 0.0) * t / 1000) / 1000
        val a = init(frame.a) + initDt.getOrElse(frame.a, 0.0) * t / 1000
        val i = init(frame.i) + initDt.getOrElse(frame.i, 0.0) * t / 1000
        val j = init(frame.j) + initDt.getOrElse(frame.j, 0.0) * t / 1000
        val k = init(frame.k) + initDt.getOrElse(frame.k, 0.0) * t / 1000
        val q = Quaternion(a, i, j, k)
        orientation = Angle.thetaFromQuaternion(q)
        //Logger("GenericRobot", Error, this.toString)
      } else {
        x = init(frame.x) / 1000
        y = init(frame.y) / 1000
        val a = init(frame.a)
        val i = init(frame.i)
        val j = init(frame.j)
        val k = init(frame.k)
        val q = Quaternion(a, i, j, k)
        orientation = Angle.thetaFromQuaternion(q)
      }
    } catch {
      case e: Throwable =>
        Logger("GenericRobot", Error, "could not compute motion") //TODO print more
        throw e
    }
  }

//override protected def moveFor(t: Int) = {
//  try {
//    val tolerance = 1e-16
//    val (init, initDt) = initSolution(tolerance)
//    val (_, value, valueDt) = ida.solve( t / 1000.0, store.mapValues(_.toDouble), init, initDt)
//    x = value(frame.x) / 1000.0
//    y = value(frame.y) / 1000.0 
//    //z = value(frame.z) / 1000.0
//    val q = Quaternion(value(frame.a), value(frame.i), value(frame.j), value(frame.k))
//    orientation = Angle.thetaFromQuaternion(q)
//    //TODO should we save the dt and other value to solve the next thing ??
//  } catch {
//    case e: Throwable =>
//      Logger("GenericRobot", Error, "could not compute motion") //TODO print more
//      throw e
//  }
//}
  
  override def elapseBP(t: Int): BranchingPoint = {

    new BranchingPoint {
      def alternatives = 1
  
      //corners of cube and center
      def act(alt: Int): List[String] = {
        moveFor(t)
        List("elapse("+t+", "+alt+")")
      }
    }
  }

  var store = Map[Variable, Short](inputs.map( _.v -> (0: Short)):_*)
  
  override def register(exec: Executor) {
    super.register(exec)

    for(i <- inputs) {
      val listener = new MessageListenerRW[std_msgs.Int16]{
        def robotID = id
        override def read = Some(Set())
        override def written = Some(Set(i.v.toString))
        val name = i.topic
        def onNewMessage(message: std_msgs.Int16) {
          lock.lock
          try {
            store += i.v -> message.getData
          } finally lock.unlock
          exec.messageDelivered
        }
      }
      val sub = exec.getSubscriber[std_msgs.Int16](id + "/" + i.topic, std_msgs.Int16._TYPE)
      sub.addMessageListener(listener)
    }

  }
  
  override def deregister(exec: Executor) {
    super.deregister(exec)
    //TODO deregister the inputs
  }

  override def frames: List[(Frame,Box2D)] = List(frame -> bBox)

  override def variablesAt(index: Int): Map[Variable,Variable] = {
    val vars: List[Variable] = inputs.map(_.v) ++ dynamic
    vars.map( v => v -> Variable("step_" + index + "_" + v.name).setType(v.tpe) ).toMap
  }

  override def stateEquations(index: Int): Formula = {
    val vAt = variablesAt(index)
    val known = getKnown.toList.map{ case (k, l) => Eq(k, l) }
    And(known:_*).alpha(vAt)
  }

  override def unrollEquations(fromIndex: Int): Formula = {
    //val vf = variablesAt(fromIndex)
    val toIndex = fromIndex + 1
    val vt = variablesAt(toIndex)
    val cstr = dRealInitEquations
    assert(!hasDt(cstr), "dt not yet implemented")
    val cstr2 = cstr.alpha(vt)
    Logger("GenericRobot", Debug, "unrollEquations("+fromIndex+")\n" + cstr2)
    cstr2
  }

}

object GenericRobot {

  def mkVar(s: String) = Variable(s).setType(Real)

  def preprocess(content: String): (Iterable[Variable], Iterable[SExpr]) = {
    val param = "~~~ Parameters:"
    val eqns = "~~~ Equations:"
    val pStart = content.indexOf(param)
    val eStart = content.indexOf(eqns)
    assert(pStart >= 0, "no parameters")
    val ps = content.substring(pStart+param.length, eStart).split("\\n").map(_.trim).filter(_ != "")
    val vs = ps.map( v => Variable(v).setType(Real) )
    assert(eStart >= 0, "no equations")
    val es = content.substring(eStart+eqns.length)
    SExprParser.parse(es) match {
      case Some(lst) => (vs, lst)
      case None =>
        Logger.logAndThrow("GenericRobot", Error, "could not parse:\n" + es)
    }
  }

  def apply(id: String, pg: Playground, fileName: String): GenericRobot = {
    val content = IO.readTextFile(fileName)
    val (vars, sexprs)= preprocess(content)
    apply(id, pg, vars, sexprs)
  }

  def apply(id: String, pg: Playground, variables: Iterable[Variable], sexprs: Iterable[SExpr]): GenericRobot = {
    
    var bBox = List[Box2D]()
    var inputs = List[Input]()
    var dynamic = List[Variable]()
    var cstrs = List[Formula]()
    var frame = Frame(mkVar("x"), mkVar("y"), mkVar("z"), mkVar("a"), mkVar("i"), mkVar("j"), mkVar("k"))

    for (se <- sexprs) {
      se match {
        case SApplication("bbox", List(SAtom(x), SAtom(y), SAtom(w), SAtom(h), SAtom(theta))) =>
          bBox ::= new Box2D(x.toDouble / 1000.0, y.toDouble / 1000.0, theta.toDouble / 1000.0,
                             w.toDouble / 1000.0, h.toDouble / 1000.0)
        case SApplication("frame", List(SAtom(x), SAtom(y), SAtom(z), SAtom(a), SAtom(i), SAtom(j), SAtom(k))) =>
          frame = Frame(mkVar(x), mkVar(y), mkVar(z), mkVar(a), mkVar(i), mkVar(j), mkVar(k))
        case SApplication("input", List(SAtom(name), SAtom(topic))) =>
          inputs ::= Input(mkVar(name), topic)
        case SApplication("input", List(SAtom(name))) =>
          inputs ::= Input(mkVar(name), name)
        case SApplication("dynamic", List(SAtom(name))) =>
          dynamic ::= mkVar(name)
      //case Application("input", List(Atom(name), Atom(port))) =>
      //  inputs ::= Input(Variable(name).setType(Real), port)
      //case Application("output", List(Atom(name), formula)) =>
      //  val v = Variable(name + "Primed").setType(Real) //TODO really ?
      //  cstrs ::= Eq(v, parseFormula(formula))
        case SApplication("assert", List(formula)) =>
          cstrs ::= parseFormula(formula)
        case other =>
          cstrs ::= parseFormula(other)
      }
    }


    val bb = bBox match {
      case Nil =>
        Logger("GenericRobot", Warning, "does not have a bounding box")
        new Box2D(-0.5, 0.5, 0, 1, 1)
        //Logger.logAndThrow("GenericRobot", Error, "need at least one bounding box")
      case b :: Nil => b
      case b :: _ =>
        Logger("GenericRobot", Warning, "TODO aggregate bounding boxes")
        b
    }

    new GenericRobot(id, pg, bb, frame, inputs, dynamic, And(cstrs:_*))
  }

}
