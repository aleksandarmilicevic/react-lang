package react.verification.model.generic

import react.message.Primitive
import react.runtime.MessageListenerRW
import react.Executor
import react.robot._
import react.verification.model._
import react.verification.environment._
import dzufferey.smtlib._
import dzufferey.utils._
import dzufferey.utils.LogLevel._

case class Input(v: Variable, topic: String)
case class Fun(name: String, args: List[Variable], body: Formula)

class GenericRobot( bBox: Box2D, inputs: List[Input], fcts: List[Fun], constraints: Formula) extends GroundRobot(bBox, None) {

  override protected def moveFor(t: Int) = {
    ???
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

  def apply(sexprs: List[SExpr]): GenericRobot = {
    
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
          val v = Variable(name).setType(Real)
          cstrs ::= Eq(v, parseFormula(formula))
        case Application("assert", List(formula)) =>
          cstrs ::= Eq(parseFormula(formula))
        case Application("let", List(Atom(name), args, formula)) =>
          val args2 = args match {
            case Application(i1, is) => (i1 :: is).map{
              case Atom(n) => Variable(n).setType(Real)
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

    new GenericRobot(bb, inputs, funs, And.application(cstrs))
  }

}
