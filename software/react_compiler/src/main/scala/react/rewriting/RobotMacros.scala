package react.rewriting

import scala.language.experimental.macros
//import scala.reflect.macros.whitebox.Context
import scala.reflect.macros.blackbox.Context

class RobotMacros(val c: Context) extends Handlers
{

  import c.universe._

  class SymbolGetter extends Traverser {
    var acc = List[Symbol]()
    override def traverse(tree: Tree): Unit = tree match {
      case Apply(fun, args) =>
        acc = fun.symbol :: acc
        super.traverse(tree)
      case Assign(lhs, rhs) =>
        acc = lhs.symbol :: acc
        super.traverse(tree)
      case id @ Ident(_) =>
        acc = id.symbol :: acc
        super.traverse(tree)
      case _ =>
        super.traverse(tree)
    }
  }

  def getGetter(t: Tree): List[TermSymbol] = {
    val sg = new SymbolGetter
    sg.traverse(t)
    val syms = sg.acc
    val accessed = syms.collect{
      case t: TermSymbol if t.isGetter =>
        t.accessed.asInstanceOf[TermSymbol]
    }
    accessed.toList
  }

  def getSetter(t: Tree): List[TermSymbol] = {
    val sg = new SymbolGetter
    sg.traverse(t)
    val accessed = sg.acc.collect{
      case t: TermSymbol if t.isSetter =>
        t.accessed.asInstanceOf[TermSymbol]
    }
    accessed.toList
  }

  def varSymToString(s: TermSymbol) = s.getter.name.toString

  def registerHandler[T <: react.message.Message : c.WeakTypeTag]
    (source: c.Expr[String])
    (handler: c.Expr[PartialFunction[T, Unit]]): c.Expr[Unit] =
  {
    val rosName = convertMsgName( weakTypeOf[T] )
    val rosType = convertMsgType( weakTypeOf[T] )

    val readVars = getGetter(handler.tree).map(varSymToString).toSet
    val writtenVars = getSetter(handler.tree).map(varSymToString).toSet

    //TODO we assume that handlers do not forward messages ...
    
    val tree = q"""{
      val rw = new react.runtime.RW {
        def robotID = ???
        override def read = Some($readVars)
        override def written = Some($writtenVars)
        override def sendMsgsTo = Some(Set())
      }
      val h = $handler
      subscribe[$rosType]($source, $rosName, Some(rw))( (message: $rosType) => {
        val msg = react.message.Message.from(message)
        if (h.isDefinedAt(msg)) { h.apply(msg) }
      })
    }"""
    c.Expr[Unit](tree)
  }
  
  def publish[T <: react.message.Message : c.WeakTypeTag]
    (topic: c.Expr[String], message: c.Expr[T]): c.Expr[Unit] =
  {
    val rosName = convertMsgName( weakTypeOf[T] )
    val rosType = convertMsgType( weakTypeOf[T] )

    val tree = q"publish[$rosType]($topic, $rosName, exec.convertMessage[$rosType]($message))"
    c.Expr[Unit](tree)
  }

  def every(period: c.Expr[Int])(body: c.Expr[Unit]): c.Expr[Unit] = {
    val tree = q"addTask(new react.runtime.ScheduledTask(id, $period, (() => $body)))"
    val tree2 = c.untypecheck(tree)
    c.Expr[Unit](tree2)
  }
  
  def makeState(sid: c.Expr[scala.Symbol])(body: c.Expr[Unit]): c.Expr[Unit] = {
    val tree = q"""{
      assert(!(statesMap.contains($sid)), "redefinitions of " + $sid)
      snapshotData
      val state = new State($sid) { $body }
      fixData(state)
      statesMap += ($sid -> state)
    }"""
    //val tree2 = c.untypecheck(tree)
    c.Expr[Unit](tree)
  }

}

