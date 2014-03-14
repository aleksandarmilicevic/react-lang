package react.ast

import scala.text.Document
import scala.text.Document._

//TODO pretty printing utils
//formats:
// -text
// -html (graphviz for FSM structure ?)

class Printer {

  sealed abstract class Affix
  case object Prefix extends Affix
  case object Infix extends Affix
  case object Suffix extends Affix

  def affix(s: Symbol): Affix = s match {
    case Call(_) | UMinus | Not => Prefix
    case Tuple | Plus | Minus | Times | Divides
       | Modulo | And | Or | Equal | Lt => Infix
  }

  def priority(s: Symbol): Int = s match {
    case _ => sys.error("TODO")
  }
  
  def priority(s: Expr): Int = s match {
    case _ => sys.error("TODO")
  }

  def apply(x: Id): Document = {
    ((empty: Document) /: x.path)( (acc, id) => id :: "." :: acc)
  }

  def apply(x: Symbol): Document = x match {
    case Call(id) => apply(id)
    case Tuple => text(",")
    case UMinus => text("-")
    case Not =>  text("¬")
    case Plus => text("+")
    case Minus => text("-")
    case Times => text("*")
    case Divides => text("/")
    case Modulo => text("%")
    case And => text("∧")
    case Or => text("∨")
    case Equal => text("=")
    case Lt => text("<")
  }
  
  def apply(x: LHS): Document = x match {
    case _ => sys.error("TODO")
  }

  def apply(x: Expr, currPriority: Int = -1): Document = x match {
    case _ => sys.error("TODO")
  }
  
  def apply(x: Stmnt): Document = x match {
    case _ => sys.error("TODO")
  }

  def apply(x: Handler): Document = x match {
    case _ => sys.error("TODO")
  }

  def apply(x: State): Document = {
    sys.error("TODO")
  }

  def apply(x: Context): Document = {
    sys.error("TODO")
  }

  def apply(x: CompilationUnit): Document = {
    sys.error("TODO")
  }

  def apply(x: Program): Document = {
    sys.error("TODO")
  }

}

object HtmlPrinter extends Printer {
  //TODO
}
