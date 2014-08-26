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
    val s = size(world)
    c.Expr[Int](q"$s")
  }

}
