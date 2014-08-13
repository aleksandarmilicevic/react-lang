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
    val reactType = weakTypeOf[T]

    val sub = q"""val sub: org.ros.node.topic.Subscriber[$rosType] = node.newSubscriber(react.utils.RosUtils.mayAddPrefix(id, $source), $rosName)"""
    val list = q"""val listener: org.ros.message.MessageListener[$rosType] = new org.ros.message.MessageListener[$rosType]{
          val h = $handler
          def onNewMessage(message: $rosType) {
            val msg = react.message.Message.fromMessage($rosName, message).asInstanceOf[$reactType]
            if (h.isDefinedAt(msg)) {
              lock.lock()
              try {
                h.apply(msg)
              } finally {
                lock.unlock
              }
            }
          }
        }"""
    val body = q"""{
          $sub
          $list
          sub.addMessageListener(listener)
        }"""
    val tree = q"( (node: org.ros.node.ConnectedNode) => $body )"

    val tree2 = q"sensors = $tree :: sensors"
    //Console.err.println("generated handler:\n" + tree2)
    c.Expr[Unit](tree2)
  }
  
  //TODO cache the publisher
  def publish[T <: react.message.Message : c.WeakTypeTag]
    (topic: c.Expr[String], message: c.Expr[T]): c.Expr[Unit] =
  {
    val rosName = convertMsgName( weakTypeOf[T] )
    val rosType = convertMsgType( weakTypeOf[T] )

    val tree = q"""
{
  val pub = getPublisher[$rosType]($topic, $rosName)
  val msg = react.message.Message.toMessage(node, $message).asInstanceOf[$rosType]
  pub.publish(msg)
}
"""
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
