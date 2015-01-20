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

  val xIntervals: List[Double] = {
    val steps = ((pg.xMax - pg.xMin) / pg.xDiscretization).toInt
    (for (i <- 0 to steps) yield pg.xMin + i * pg.xDiscretization).toList
  }

  val yIntervals: List[Double] = {
    val steps = ((pg.yMax - pg.yMin) / pg.yDiscretization).toInt
    (for (i <- 0 to steps) yield pg.yMin + i * pg.yDiscretization).toList
  }
  
  val yawIntervals: List[Double] = {
    val steps = (2 * math.Pi / pg.fpDiscretization).toInt
    (for (i <- 0 to steps) yield -math.Pi + i * pg.fpDiscretization).toList
  }
  
  protected val timeVar = Variable("t").setType(Real)
  protected val xVar    = Variable("x").setType(Real)
  protected val yVar    = Variable("y").setType(Real)
  protected val yawVar  = Variable("yaw").setType(Real)
  protected val xVar1   = Variable("xPrimed").setType(Real)
  protected val yVar1   = Variable("yPrimed").setType(Real)
  protected val yawVar1 = Variable("yawPrimed").setType(Real)
  

  def baseConstraints = {
    val funMap = fcts.map(f => f.name -> f).toMap
    def substFun(formula: Formula): Formula = formula match {
      case dzufferey.smtlib.Application(UnInterpretedFct(name,_,_), args) if funMap contains name =>
        val applied = funMap(name)(args)
        FormulaUtils.map(substFun, applied)
      case f => f
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
    val in = inputs.flatMap( i => store.get(i.v).map( v => Eq(i.v, Literal(v.toDouble))) )
    val pos = List(
      Eq(xVar, Literal(x)), //TODO discretization
      Eq(yVar, Literal(y)), //TODO discretization
      Eq(yawVar, Literal(orientation)) //TODO discretization
    )
    val allCstrs = dzufferey.smtlib.Application(And, constraints :: pos ::: in).setType(Bool)
    val withDefs = FormulaUtils.map(substFun, allCstrs)
    fixTypes(withDefs)
    withDefs 
  }

  protected def getMin(t: Double, v: Variable, intervals: List[Double]): Option[Double] = {
    val cstr = baseConstraints
    intervals.find( value => {
      val cutting = Lt(v, Literal(value))
      val solver = DReal(QF_NRA)
      //val time = Eq(timeVar, Divides(Literal(t), Literal(1000.0))) //TODO Int vs Real
      val time = Eq(timeVar, Literal(t/1000.0))
      val res = solver.testB(And( time, And(baseConstraints, cutting)))
      //Logger("GenericRobot", Notice, v + " < " + value + " → " + res)
      res
    })
  }

  protected def getMax(t: Double, v: Variable, intervals: List[Double]): Option[Double] = {
    val cstr = baseConstraints
    intervals.reverse.find( value => {
      val cutting = Gt(v, Literal(value))
      val solver = DReal(QF_NRA)
      //val time = Eq(timeVar, Divides(Literal(t), Literal(1000.0))) //TODO Int vs Real
      val time = Eq(timeVar, Literal(t/1000.0))
      val res = solver.testB(And( time, And(baseConstraints, cutting)))
      //Logger("GenericRobot", Notice, v + " > " + value + " → " + res)
      res
    })
  }

  //TODO make that cleaner and figure out how δ-sat influence > vs ≥

  protected def getMinX(t: Double): Double = {
    getMin(t, xVar1, xIntervals).getOrElse(pg.xMax.toDouble) //- pg.xDiscretization
  }

  protected def getMaxX(t: Double): Double = {
    getMax(t, xVar1, xIntervals).getOrElse(pg.xMin.toDouble) //+ pg.xDiscretization
  }
  
  protected def getMinY(t: Double): Double = {
    getMin(t, yVar1, yIntervals).getOrElse(pg.yMax.toDouble) //- pg.yDiscretization
  }

  protected def getMaxY(t: Double): Double = {
    getMax(t, yVar1, yIntervals).getOrElse(pg.yMin.toDouble) //+ pg.yDiscretization
  }
  
  protected def getMinYaw(t: Double): Double = {
    getMin(t, yawVar1, yawIntervals).getOrElse(10.0) - pg.fpDiscretization
  }

  protected def getMaxYaw(t: Double): Double = {
    getMax(t, yawVar1, yawIntervals).getOrElse(-10.0) //+ pg.fpDiscretization
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

  def parseFormula(e: SExpr): Formula = e match {
    case Atom(str) =>
      Misc.toDouble(str) match {
        case Some(l) => Literal(l)
        case None => Variable(str).setType(Real)
      }
    case Application(op, args) =>
      val args2 = args map parseFormula
      val symbol: Symbol = op match {
        case "der" =>
          DRealDecl.timeDerivative
        case _ if InterpretedFct(op).isDefined =>
          InterpretedFct(op).get
        case _ if DRealDecl.fcts.exists(_.toString == op) =>
          DRealDecl.fcts.find(_.toString == op).get
        case other =>
          UnInterpretedFct(other)
      }
      symbol.application(args2)
    case SNil => 
      Logger.logAndThrow("GenericRobot", Error, "expected expression, not ()")
  }

  def apply(pg: Playground, fileName: String): GenericRobot = {
    val content = IO.readTextFile(fileName)
    val sexpres = SExprParser.parse(content)
    sexpres match {
      case Some(lst) => apply(pg, lst)
      case None =>
        Logger.logAndThrow("GenericRobot", Error, "could not parse:\n" + content)
    }
  }

  def apply(pg: Playground, sexprs: List[SExpr]): GenericRobot = {
    
    var bBox = List[Box2D]()
    var inputs = List[Input]()
    var funs = List[Fun]()
    var cstrs = List[Formula]()

    for (se <- sexprs) {
      se match {
        case Application("bbox", List(Atom(x), Atom(y), Atom(w), Atom(h), Atom(theta))) =>
          bBox ::= new Box2D(x.toDouble, y.toDouble, theta.toDouble, w.toDouble, h.toDouble)
        case Application("input", List(Atom(name), Atom(port))) =>
          inputs ::= Input(Variable(name).setType(Real), port)
        case Application("output", List(Atom(name), formula)) =>
          val v = Variable(name + "Primed").setType(Real) //TODO really ?
          cstrs ::= Eq(v, parseFormula(formula))
        case Application("assert", List(formula)) =>
          cstrs ::= parseFormula(formula)
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
        case Application(_, _) =>
          Logger("GenericRobot", Warning, "unexpected/ill-formed s-expr: " + se)
        case Atom(e) =>
          Logger("GenericRobot", Warning, "dangling atom: " + e)
        case SNil =>
          Logger("GenericRobot", Warning, "unexpected: ()")
      }
    }

    val bb = bBox match {
      case Nil =>
        Logger.logAndThrow("GenericRobot", Error, "need at least one bounding box")
      case b :: Nil => b
      case b :: _ =>
        Logger("GenericRobot", Warning, "TODO aggregate bounding boxes")
        b
    }

    new GenericRobot(pg, bb, inputs, funs, And.application(cstrs))
  }

}
