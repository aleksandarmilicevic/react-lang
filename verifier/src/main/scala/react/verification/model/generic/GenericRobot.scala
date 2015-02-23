package react.verification.model.generic

import react.message.Primitive
import react.runtime.MessageListenerRW
import react.Executor
import react.robot._
import react.verification.Playground
import react.verification.model._
import react.verification.environment._
import react.verification.modelchecker.BranchingPoint
import dzufferey.smtlib._
import dzufferey.utils._
import dzufferey.utils.LogLevel._
import Utils._

case class Input(v: Variable, topic: String)
case class Fun(name: String, args: List[Variable], body: Formula) {
  def apply(a: List[Formula]) = {
    assert(a.length == args.length, "wrong arity when applying " + name)
    val subst = args.zip(a).toMap[Formula, Formula]
    def fct(f: Formula): Formula = subst.getOrElse(f, f)
    FormulaUtils.map(fct, body)
  }
}

class GenericRobot( pg: Playground,
                    bBox: Box2D,
                    inputs: List[Input],
                    fcts: List[Fun],
                    constraints: Formula ) extends GroundRobot(bBox, None) {

  override protected def moveFor(t: Int) = {
    Logger.logAndThrow("GenericRobot", Error, "should not be used")
  }

  val xIntervals: Array[Double] = {
    val steps = ((pg.xMax - pg.xMin) / pg.xDiscretization).toInt
    (for (i <- 0 to steps) yield pg.xMin + i * pg.xDiscretization).toArray
  }

  val yIntervals: Array[Double] = {
    val steps = ((pg.yMax - pg.yMin) / pg.yDiscretization).toInt
    (for (i <- 0 to steps) yield pg.yMin + i * pg.yDiscretization).toArray
  }
  
  val yawIntervals: Array[Double] = {
    val steps = (2 * math.Pi / pg.fpDiscretization).toInt
    (for (i <- 0 to steps) yield -math.Pi + i * pg.fpDiscretization).toArray
  }
  
  protected val timeVar = Variable("t").setType(Real)
  protected val xVar    = Variable("x").setType(Real)
  protected val yVar    = Variable("y").setType(Real)
  protected val yawVar  = Variable("yaw").setType(Real)
  //TODO not like that anymore ...
  //beam.dx/y/z is the position of the seg
  protected val xVar1   = Variable("xPrimed").setType(Real)
  protected val yVar1   = Variable("yPrimed").setType(Real)
  protected val yawVar1 = Variable("yawPrimed").setType(Real)
  

  //TODO we need to add bounds to the variables
  def baseConstraints = {
    val funMap = fcts.map(f => f.name -> f).toMap
    def substFun(formula: Formula): Formula = formula match {
      case dzufferey.smtlib.Application(UnInterpretedFct(name,_,_), args) if funMap contains name =>
        val applied = funMap(name)(args)
        FormulaUtils.map(substFun, applied)
      case f => f
    }
    val in = inputs.flatMap( i => store.get(i.v).map( v => Eq(i.v, Literal(v.toDouble))) )
    val pos = List(
      Eq(xVar, Literal(x)), //TODO discretization
      Eq(yVar, Literal(y)), //TODO discretization
      Eq(yawVar, Literal(orientation)) //TODO discretization
    )
    val bounds = List(
      Lt(xVar1, Literal(pg.xMax + pg.xDiscretization)),
      Gt(xVar1, Literal(pg.xMin - pg.xDiscretization)),
      Lt(yVar1, Literal(pg.yMax + pg.yDiscretization)),
      Gt(yVar1, Literal(pg.yMin - pg.yDiscretization)),
      Lt(yawVar1, Literal(math.Pi + pg.fpDiscretization)),
      Gt(yawVar1, Literal(-math.Pi - pg.fpDiscretization))
    )
    val allCstrs = And((constraints :: pos ::: bounds ::: in):_*)
    val withDefs = FormulaUtils.map(substFun, allCstrs)
    fixTypes(withDefs)
    withDefs 
  }

  //find the first sat value
  protected def firstSatSearch(mkFormula: Double => Formula, seq: Array[Double]): Option[Double] = {
    //var cnt = 0
    def test(idx: Int) = {
      //cnt += 1
      val solver = DReal(QF_NRA)//, "search_" + cnt + ".smt2")
      solver.testB(mkFormula(seq(idx)))
    }
    var lo = 0
    var hi = seq.length - 1
    var isSat = false 
    while (lo < hi) {
      val middle = lo + (hi-lo)/2
      assert(middle < hi, "no progress")
      if (test(middle)) {
        //println("sat at " + middle + ", " + seq(middle) )
        hi = middle
      } else {
        //println("unsat at " + middle + ", " + seq(middle) )
        lo = middle + 1
      }
    }
    val res1 = if (lo == hi && test(lo)) {
      Some(seq(lo))
    } else {
      None
    }
    ////to double check
    //val res2 = seq.find( value => {
    //  val solver = DReal(QF_NRA)
    //  val formula = mkFormula(value)
    //  val res = solver.testB(formula)
    //  res
    //})
    //assert(res1 == res2, "res1: " + res1 + ", res2: " + res2 + ", seq: " + seq.mkString("[",",","]") + ", lo: " + lo + ", hi: " + hi)
    res1
  }

  protected def getMin(t: Double, v: Variable, intervals: Array[Double]): Option[Double] = {
    val time = Eq(timeVar, Divides(Literal(t), Literal(1000.0)).setType(Real)) //Int vs Real
    val cstr = And(time, baseConstraints)
    def mkFormula(value: Double) = And(cstr, Leq(v, Literal(value)))
    firstSatSearch(mkFormula, intervals)
  }

  protected def getMax(t: Double, v: Variable, intervals: Array[Double]): Option[Double] = {
    val time = Eq(timeVar, Divides(Literal(t), Literal(1000.0)).setType(Real)) //Int vs Real
    val cstr = And(time, baseConstraints)
    def mkFormula(value: Double) = And(cstr, Geq(v, Literal(value)))
    firstSatSearch(mkFormula, intervals.reverse)
  }

  //TODO make that cleaner and figure out how δ-sat influence > vs ≥

  protected def getMinX(t: Double): Double = {
    val res = getMin(t, xVar1, xIntervals)
    //println("getMinX: " + res)
    res.getOrElse(pg.xMax.toDouble) //- pg.xDiscretization
  }

  protected def getMaxX(t: Double): Double = {
    val res = getMax(t, xVar1, xIntervals)
    //println("getMaxX: " + res)
    res.getOrElse(pg.xMin.toDouble) //+ pg.xDiscretization
  }
  
  protected def getMinY(t: Double): Double = {
    val res = getMin(t, yVar1, yIntervals)
    //println("getMinY: " + res)
    res.getOrElse(pg.yMax.toDouble) //- pg.yDiscretization
  }

  protected def getMaxY(t: Double): Double = {
    val res = getMax(t, yVar1, yIntervals)
    //println("getMaxY: " + res)
    res.getOrElse(pg.yMin.toDouble) //+ pg.yDiscretization
  }
  
  protected def getMinYaw(t: Double): Double = {
    val res = getMin(t, yawVar1, yawIntervals)
    //println("getMinΘ: " + res)
    res.getOrElse(10.0) - pg.fpDiscretization
  }

  protected def getMaxYaw(t: Double): Double = {
    val res = getMax(t, yawVar1, yawIntervals)
    //println("getMaxΘ: " + res)
    res.getOrElse(-10.0) //+ pg.fpDiscretization
  }
  
  override def elapseBP(t: Int): BranchingPoint = {

    val minX: Double = getMinX(t)
    val maxX: Double = getMaxX(t)
    val minY: Double = getMinY(t)
    val maxY: Double = getMaxY(t)
    val minYaw: Double = getMinYaw(t)
    val maxYaw: Double = getMaxYaw(t)
      
    Logger("GenericRobot", Notice, "minX: " + minX + ", maxX: " + maxX)
    Logger("GenericRobot", Notice, "minY: " + minY + ", maxY: " + maxY)
    Logger("GenericRobot", Notice, "minΘ: " + minYaw + ", maxΘ: " + maxYaw)

    val xs = if (minX < maxX) 2
             else if (minX == maxX) 1
             else 0
    val ys = if (minY < maxY) 2
             else if (minY == maxY) 1
             else 0
    val yaws = if (minYaw < maxYaw) 2
             else if (minYaw == maxYaw) 1
             else 0
    
    val alt =
      if (xs*ys*yaws == 1) 1
      else if (xs*ys*yaws > 1) xs*ys*yaws + 1
      else Logger.logAndThrow("GenericRobot", Error, "movement equations do not have a solution ?!!")

    new BranchingPoint {
      def alternatives = alt
  
      //corners of cube and center
      def act(alt: Int): List[String] = {
        if (yaws == 1) 
          alt match {
            case 1 => x = minX; y = minY; orientation = (maxYaw - minYaw)/2
            case 2 => x = maxX; y = maxY; orientation = (maxYaw - minYaw)/2
            case 3 => x = minX; y = maxY; orientation = (maxYaw - minYaw)/2
            case 4 => x = maxX; y = minY; orientation = (maxYaw - minYaw)/2
            case _ => x = (minX + maxX)/2; y = (minY + maxY)/2; orientation = (maxYaw - minYaw)/2
          }
        else
          alt match {
            case 1 => x = minX; y = minY; orientation = minYaw
            case 2 => x = maxX; y = maxY; orientation = minYaw
            case 3 => x = minX; y = maxY; orientation = minYaw
            case 4 => x = maxX; y = minY; orientation = minYaw
            case 5 => x = minX; y = minY; orientation = maxYaw
            case 6 => x = maxX; y = maxY; orientation = maxYaw
            case 7 => x = minX; y = maxY; orientation = maxYaw
            case 8 => x = maxX; y = minY; orientation = maxYaw
            case _ => x = (minX + maxX)/2; y = (minY + maxY)/2; orientation = (maxYaw - minYaw)/2
          }
        List("elapse("+t+", "+alt+")")
      }
    }
  }

  var store = Map[Variable, Short]()
  
  override def register(exec: Executor) {
    super.register(exec)

    for(i <- inputs) {
      val listener = new MessageListenerRW[std_msgs.Int16]{
        def robotID = GenericRobot.this.toString //TODO better
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
      val sub = exec.getSubscriber[std_msgs.Int16](i.topic, std_msgs.Int16._TYPE)
      sub.addMessageListener(listener)
    }

  }
  
  override def deregister(exec: Executor) {
    super.deregister(exec)
    //TODO deregister the inputs
  }

}

object GenericRobot {

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

  def apply(pg: Playground, fileName: String): GenericRobot = {
    val content = IO.readTextFile(fileName)
    val (vars, sexprs)= preprocess(content)
    apply(pg, vars, sexprs)
  }

  def apply(pg: Playground, variables: Iterable[Variable], sexprs: Iterable[SExpr]): GenericRobot = {
    
    var bBox = List[Box2D]()
    var inputs = List[Input]()
    var dynamic = List[Variable]()
    var funs = List[Fun]()
    var cstrs = List[Formula]()

    for (se <- sexprs) {
      se match {
        case Application("bbox", List(Atom(x), Atom(y), Atom(w), Atom(h), Atom(theta))) =>
          bBox ::= new Box2D(x.toDouble, y.toDouble, theta.toDouble, w.toDouble, h.toDouble)
        case Application("input", List(Atom(name))) =>
          inputs ::= Input(Variable(name).setType(Real), "") //TODO generate a port name
        case Application("dynamic", List(Atom(name))) =>
          dynamic ::= Variable(name).setType(Real)
      //case Application("input", List(Atom(name), Atom(port))) =>
      //  inputs ::= Input(Variable(name).setType(Real), port)
      //case Application("output", List(Atom(name), formula)) =>
      //  val v = Variable(name + "Primed").setType(Real) //TODO really ?
      //  cstrs ::= Eq(v, parseFormula(formula))
        case Application("assert", List(formula)) =>
          cstrs ::= parseFormula(formula)
        //TODO that thing might not be needed after all
        case Application("let", List(Atom(name), args, formula)) =>
          val args2 = args match {
            case Application(i1, is) => (Atom(i1) :: is).flatMap{
              case Atom(n) => List(Variable(n).setType(Real))
              case SNil => Nil
              case e => Logger.logAndThrow("GenericRobot", Error, "argument should be atom, not " + e)
            }
            case SNil => Nil
            case Atom(e) =>
              Logger.logAndThrow("GenericRobot", Error, "expected list of arguments, not " + e)
          }
          funs ::= Fun(name, args2, parseFormula(formula))
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

    new GenericRobot(pg, bb, inputs, funs, And(cstrs:_*))
  }

}
