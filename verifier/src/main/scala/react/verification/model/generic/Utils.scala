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
    case SAtom("pi") => Literal(math.Pi)
    case SAtom(str) =>
      Misc.toDouble(str) match {
        case Some(l) => Literal(l)
        case None => Variable(str).setType(Real)
      }
    case SApplication(op, args) =>
      val args2 = args map parseFormula
      val symbol: Symbol = op match {
        case "D" =>
          DRealDecl.timeDerivative
        case _ if InterpretedFct(op).isDefined =>
          //println("known symbol: " + op)
          InterpretedFct(op).get
        case _ if DRealDecl.fcts.exists(_.toString == op) =>
          //println("known symbol: " + op)
          DRealDecl.fcts.find(_.toString == op).get
        case other =>
          Logger("GenericRobot", Debug, "unknown symbol: " + op)
          UnInterpretedFct(other)
      }
      (symbol, args2) match {
        case (Minus, List(Literal(d: Double))) => Literal(-d)
        case _ => symbol(args2:_*)
      }
    case SNil => 
      Logger.logAndThrow("GenericRobot", Error, "expected expression, not ()")
  }

  def occursInDerivative(v: Variable, f: Formula): Boolean = {
    FormulaUtils.collect(false, (acc: Boolean, f) => f match {
      case Application(DRealDecl.timeDerivative, args) =>
        if (And(args:_*).freeVariables contains v) true else acc
      case _ => acc
    }, f)
  }

  def hasDt(f: Formula) = {
    FormulaUtils.collectSymbols(f).contains(DRealDecl.timeDerivative)
  }
  

}
