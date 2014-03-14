package react.ast

import scala.util.parsing.input.Positional

sealed abstract class Stmnt extends Positional
case class ITE(cond: Expr, caseTrue: Stmnt, caseFalse: Stmnt) extends Stmnt
case class While(cond: Expr, body: Stmnt) extends Stmnt
case class Block(body: List[Stmnt]) extends Stmnt
case class Return(e: Expr) extends Stmnt
case class Affect(lhs: LHS, e: Expr) extends Stmnt
case class Send(dest: Expr, msg: Expr) extends Stmnt //TODO flush and other mailbox operation ?
case class Let(lhs: Id, e: Expr, mutable: Boolean) extends Stmnt //let bindings are declarations