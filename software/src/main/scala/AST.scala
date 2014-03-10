package react

import scala.util.parsing.input.Positional
import react.utils.{LogCritical, LogError, LogWarning, LogNotice, LogInfo, LogDebug, Logger}


//import scala.virtualization.lms.common._

//TODO here comes LMS magic

//  trait ... extends Base {

//    //concepts
//    type 
//    def ...

//    //syntactic sugar
//    implicit class Ops(v: Rep[]) {
//      def + - *
//    }

//  }

//  trait ... extends BaseExp

//package.class.fun encoded by List(fun, class, package)
class Id(path: List[String]) {
  def addPrefix(prefix: String) = Id(prefix :: path)
}

object Id {
  def apply(path: List[String]) = new Id(path)
  def apply(path: String) = new Id(List(path))
}

sealed abstract class Symbol
//basic language feature
case object Call extends Symbol
case object Tuple extends Symbol
//case object New extends Symbol
case object Send extends Symbol
case object Receive extends Symbol
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

sealed abstract class Type extends Positional
case object TAny extends Type
case object TBool extends Type
case object TInt extends Type
case object TFloat extends Type
case object TString extends Type
case class TArray(cnt: Type) extends Type //TODO or field
case class TTuple(elts: List[Type]) extends Type
case class TFun(args: List[Type], ret: List[Type]) extends Type
//TODO record ? (and tuples as syntactic sugar for records with idx for members)
//react specific types: events, machines, ...


trait Typed {
  var tpe: Type = TAny
  def setType(t: Type): this.type = {
    tpe = t
    this
  }
}

sealed abstract class Expr extends Positional with Typed 
case class Literal(l: Any) extends Expr //TODO specialize to primitive types ?
case class Ident(id: Id) extends Expr
case class App(fct: Symbol, args: List[Expr]) extends Expr
case class New(ctor: Id, args: List[Expr]) extends Expr

sealed abstract class Pattern extends Positional with Typed
case class UnApply(id: Id, args: List[Pattern]) extends Pattern
case class Alias(id: Id, pat: Pattern) extends Pattern
case class WildCard() extends Pattern
case class PLiteral(l : Any) extends Pattern
case class Alternatives(alts: List[Pattern]) extends Pattern
//tuple? syntactic sugar or built-in ?

sealed abstract class Stmnt extends Positional
case class ITE(cond: Expr, caseTrue: Stmnt, caseFalse: Stmnt) extends Stmnt
case class While(cond: Expr, body: Stmnt) extends Stmnt
case class Block(body: List[Stmnt]) extends Stmnt
case class Return(e: Expr) extends Stmnt
case class Affect(lhs: Id, e: Expr) extends Stmnt
case class Let(lhs: Id, e: Expr, mutable: Boolean) extends Stmnt //let bindings are declarations
//TODO react specific: send ...


sealed abstract class Handler(body: Stmnt, dest: Option[Id]) extends Positional {
  def env: Map[Id,Type] = Map[Id,Type]() //patterns binds some variables
  //TODO what are the common features of Handlers ?
  //some action to be performed
  //a optional target state (where to go after executing the action)
}
case class EventHandler(pat: Pattern, body: Stmnt, dst: Id) extends Handler(body, Some(dst)) {
  override def env = Logger.logAndThrow("AST", LogError, "TODO: EventHandler.env")
}
case class TimeOutHandler(ms: Int, body: Stmnt, dst: Id) extends Handler(body, Some(dst))
case class PeriodicHandler(ms: Int, body: Stmnt) extends Handler(body, None) //every
case class ConditionHandler(cond: Expr, body: Stmnt) extends Handler(body, None) //whenever

class State(id: Id, locals: List[Let], handlers: List[Handler]) extends Positional {
  //a state is a list of handler with some extra: id, local vars, ...
}

class Context(id: Id, locals: List[Let], states: List[State], init: Id, default: List[Handler]) extends Positional {
  //a context is a list of state, an initial state, local vars, default handlers, ...
}

class CompilationUnit(fileName: String, pck: Id, imports: List[Id], contexts: List[Context])

//TODO pre-post condition, invariants
