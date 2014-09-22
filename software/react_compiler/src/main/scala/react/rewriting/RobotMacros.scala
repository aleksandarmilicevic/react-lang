package react.rewriting

import scala.language.experimental.macros
//import scala.reflect.macros.whitebox.Context
import scala.reflect.macros.blackbox.Context

class RobotMacros(val c: Context) extends Handlers
{

  import c.universe._

  def registerHandler[T <: react.message.Message : c.WeakTypeTag]
    (source: c.Expr[String])
    (handler: c.Expr[PartialFunction[T, Unit]]): c.Expr[Unit] =
  {
    val rosName = convertMsgName( weakTypeOf[T] )
    val rosType = convertMsgType( weakTypeOf[T] )

    val tree = q"""{
      val h = $handler
      subscribe[$rosType]($source, $rosName)( (message: $rosType) => {
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

