package react.ast

import scala.util.parsing.input.Positional
import react.utils.{LogCritical, LogError, LogWarning, LogNotice, LogInfo, LogDebug, Logger}

//what are the common features of Handlers ?
//some action to be performed (body)
//an optional target state, None means stay in the current state
sealed abstract class Handler(val body: Stmnt, val dest: Option[Id]) extends Positional with SpecRel {
  def env: Map[Id,Type] = Map[Id,Type]() //pattern may bind variables
  def hasDest = dest.isDefined
}
case class EventHandler(pat: Pattern, override val body: Stmnt, dst: Id) extends Handler(body, Some(dst)) {
  override def env = Logger.logAndThrow("AST", LogError, "TODO: EventHandler.env")
}
case class TimeOutHandler(ms: Int, override val body: Stmnt, dst: Id) extends Handler(body, Some(dst))
case class PeriodicHandler(ms: Int, override val body: Stmnt) extends Handler(body, None) //every
case class ConditionHandler(cond: Expr, override val body: Stmnt) extends Handler(body, None) //whenever

class State(id: Id, locals: List[Let], handlers: List[Handler]) extends Positional with SpecSt {
  //a state is a list of handler with some extra: id, local vars, ...
}

class Context(id: Id, locals: List[Let], states: List[State], default: List[Handler]) extends Positional with SpecSt {
  //a context is a list of state, an initial state, local vars, default handlers, ...
}

class CompilationUnit(fileName: String, pck: Id, imports: List[Id], contexts: List[Context])

class Program(cu: List[CompilationUnit])

//TODO pre-post condition, invariants
