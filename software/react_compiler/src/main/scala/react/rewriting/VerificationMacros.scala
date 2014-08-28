package react.rewriting

import react.verification.Playground

import scala.language.experimental.macros
//import scala.reflect.macros.whitebox.Context
import scala.reflect.macros.blackbox.Context

import java.nio.ByteBuffer


class ExplorableMacros(val c: Context) extends Types
                                       with Fields
{

  import c.universe._

  def toWord[T: c.WeakTypeTag](world: c.Expr[Playground], out: c.Expr[ByteBuffer]): c.Expr[Unit] = {
    val toStore = permanentFields
    //val wtt = weakTypeOf[T]
    //c.echo(world.tree.pos, "" + wtt + ": state is composed of " + toStore.map(fieldGetter).mkString(","))
    val storing = for (f <- toStore) yield {
      val getter = fieldGetter(f)
      write(f, out, getter)
    }
    val tree = q"""
    {
      ..$storing
    }
    """
    c.Expr[Unit](tree)
  }

  def fromWord[T: c.WeakTypeTag](world: c.Expr[Playground], in: c.Expr[ByteBuffer]): c.Expr[Unit] = {

    val restored = for (f <- permanentFields) yield {
      val setter = fieldSetter(f)
      read(f, in, setter)
    }

    val havoced = for (f <- transientFields) yield {
      val setter = fieldSetter(f)
      havoc(f, setter)
    }

    val tree = q"""
    {
      ..$restored
      ..$havoced
    }
    """
    c.Expr[Unit](tree)
  }

  def wordLength[T: c.WeakTypeTag](world: c.Expr[Playground]): c.Expr[Int] = {
    assert(unsupportedFields.isEmpty, "unsupported fields: " + unsupportedFields)
    val size = permanentFields.map(length).foldLeft(0)( _ + _ )
    //c.echo(world.tree.pos, "size is " + size)
    c.Expr[Int](q"$size")
  }

  def fieldsSaved[T: c.WeakTypeTag]: c.Expr[String] = {
    val tpe = weakTypeOf[T].toString
    val getters = permanentFields.map(fieldGetter).map(_.name).mkString(", ")
    c.Expr[String](Literal(Constant(tpe + ": " + getters)))
  }

}
