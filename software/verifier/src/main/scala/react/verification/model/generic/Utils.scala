package react.verification.model.generic

import dzufferey.smtlib._
import dzufferey.utils._
import dzufferey.utils.LogLevel._

object Utils {

  def fixTypes(f: Formula) {
    val t = new FormulaUtils.Traverser {
      override def traverse(f: Formula) {
        super.traverse(f)
        if (f.tpe == Int) f.setType(Real)
      }
    }
    t.traverse(f)
  }

  def parseFormula(e: SExpr): Formula = e match {
    case Atom("pi") => Literal(math.Pi)
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

}
