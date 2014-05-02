package react.ast

import scala.text.Document
import scala.text.Document._

//TODO pretty printing utils
//formats:
// -text
// -html (graphviz for FSM structure ?)
//need to be careful with the grouping if we want to print to Python (sensitve to whitespaces) or extends doc with forced linebreak

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

  def addParen(doc: Document) = "(" :: doc :: text(")")
  def separate(sep: Document, elts: List[Document]) = {
    if (elts.length > 1)
      elts.dropRight(1).foldRight(elts.last)(_ :: sep :/: _)
    else if (elts.length == 1)
      elts.head
    else
      empty
  }

  def priority(s: Symbol): Int = s match {
    case Call(_) => 30
    case Times | Divides | Modulo => 25 
    case UMinus | Minus | Plus => 20
    case Equal | Lt => 15
    case Not => 10
    case And | Or => 5
    case Tuple => 0
  }
  
  def priority(s: Expr): Int = s match {
    case App(s, _) => priority(s)
    case New(_, _) => 30
    case _ => 35
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
    case Ident(id) => apply(id)
    case ArrayAccess(lhs, idx) => apply(lhs) :: "[" :: apply(idx) :: text("]")
    case FieldAccess(lhs, fld) => apply(lhs) :: "->" :: apply(fld) 
  }

  def apply(x: Expr, parentPriority: Int = -1): Document = x match {
    case Literal(l) => text(l.toString)
    case App(sym, args) => sys.error("TODO don't forget paren") //need paren if priority and infix
    case New(ctorId, args) => "new" :/: apply(ctorId) :: "(" :: "TODO" :: text(")") //TODO
    case lhs: LHS => apply(lhs)
  }
  
  def apply(x: Stmnt): Document = x match {
    case ITE(cond, caseTrue, caseFalse) =>
      group("if" :/: "(" :/: apply(cond) :/: text(")")) :/:
      nest(4, apply(caseTrue)) :/:
      "else" :/:
      nest(4, apply(caseFalse))
    case While(cond, body) =>
      "while" :/: "(" :/: apply(cond) :/: text(")") :/:
      nest(4, apply(body))
    case Block(body) =>
      group(
      "{" :/: 
      nest(4, separate(text(";"), body map (apply(_)))) :/:
      text("}"))
    case Return(e) =>
      group("return" :/: apply(e))
    case Affect(lhs, e) =>
      group(apply(lhs) :/: "←" :/: apply(e))
    case Send(dest, msg) =>
      group("send" :/: apply(msg) :/: "to" :/: apply(dest))
    case Let(lhs, e, mutable) =>
      val kw = if (mutable) "var" else "let"
      group(kw :/: apply(lhs) :/: "←" :/: apply(e))
  }

  def apply(x: Handler): Document = x match {
    case EventHandler(pat, body, dst) =>
      sys.error("TODO")
    case TimeOutHandler(ms, body, dst) =>
      sys.error("TODO")
    case PeriodicHandler(ms, body) =>
      sys.error("TODO")
    case ConditionHandler(cond, body) =>
      sys.error("TODO")
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
