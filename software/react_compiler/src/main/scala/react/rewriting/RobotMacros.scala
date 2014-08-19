package react.rewriting

import react.World

import scala.language.experimental.macros
//import scala.reflect.macros.whitebox.Context
import scala.reflect.macros.blackbox.Context

import java.nio.ByteBuffer

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
    val tree = q"addTask(new react.runtime.ScheduledTask($period, (() => $body)))"
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

class ExplorableMacros(val c: Context) extends Types
                                       with Shadowing
{

  import c.universe._

  def toWord[T: c.WeakTypeTag](world: c.Expr[World], out: c.Expr[ByteBuffer]): c.Expr[Unit] = {
    val toStore = permanentFields
    val size = toStore.map(length).foldLeft(0)( _ + _ )
    val storing = for (f <- toStore) yield {
      val getter = fieldGetter(f)
      val writer = write(f)
      q"$out.$writer($getter)"
    }
    val tree = q"""
    {
      ..$storing
    }
    """
    c.Expr[Unit](tree)
  }

  def fromWord[T: c.WeakTypeTag](world: c.Expr[World], in: c.Expr[ByteBuffer]): c.Expr[Unit] = {

    val restored = for (f <- permanentFields) yield {
      val setter = fieldSetter(f)
      val reader = read(f)
      q"$setter($reader($in))"
    }

    val havoced = for (f <- transientFields) yield {
      val setter = fieldSetter(f)
      val hvc = havoc(f)
      q"$setter($hvc)"
    }

    val tree = q"""
    {
      ..$restored
      ..$havoced
    }
    """
    c.Expr[Unit](tree)
  }

  def wordLength[T: c.WeakTypeTag](world: c.Expr[World]): c.Expr[Int] = {
    val s = size(world)
    c.Expr[Int](q"$s")
  }

}
