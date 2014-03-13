package react.ast

import scala.util.parsing.input.Positional

sealed abstract class Symbol
//basic language feature
case object Call extends Symbol
case object Tuple extends Symbol
//case object New extends Symbol
//case object Send extends Symbol
//TODO flush and other mailbox operation ?
//field ref/updates
case object Read extends Symbol
case object Write extends Symbol
//arithmetic
case object UMinus extends Symbol
case object Plus extends Symbol
case object Minus extends Symbol
case object Times extends Symbol
case object Divides extends Symbol
case object Modulo extends Symbol
//bool Op
case object And extends Symbol
case object Or extends Symbol
case object Not extends Symbol
case object Equal extends Symbol
case object Lt extends Symbol

sealed abstract class Expr extends Positional with Typed 
case class Literal(l: Any) extends Expr //specialize to primitive types ?
case class App(fct: Symbol, args: List[Expr]) extends Expr
case class New(ctor: Id, args: List[Expr]) extends Expr
abstract class LHS extends Expr
case class Ident(id: Id) extends LHS
case class ArrayAccess(lhs: LHS, idx: Expr) extends LHS
case class FieldAccess(lhs: LHS, fld: Id) extends LHS

sealed abstract class Pattern extends Positional with Typed
case class UnApply(id: Id, args: List[Pattern]) extends Pattern
case class Alias(id: Id, pat: Pattern) extends Pattern
case class WildCard() extends Pattern
case class PLiteral(l : Any) extends Pattern
case class Alternatives(alts: List[Pattern]) extends Pattern
//tuple? syntactic sugar or built-in ?
