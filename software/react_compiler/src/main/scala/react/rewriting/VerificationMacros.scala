package react.rewriting

import react.verification.{Playground, Stateful}

import scala.language.experimental.macros
//import scala.reflect.macros.whitebox.Context
import scala.reflect.macros.blackbox.Context

import java.nio.ByteBuffer


class ExplorableMacros(val c: Context) extends Types
                                       with Fields
{

  import c.universe._

  def wordLength[T: c.WeakTypeTag]: c.Expr[Int] = {
    assert(unsupportedFields.isEmpty, "unsupported fields: " + unsupportedFields)
    val size = permanentFields.map(length).foldLeft(0)( _ + _ )
    c.Expr[Int](q"$size")
  }

  def fieldsSaved[T: c.WeakTypeTag]: c.Expr[String] = {
    val tpe = weakTypeOf[T].toString
    val getters = permanentFields.map(fieldGetter).map(_.name).mkString(", ")
    c.Expr[String](Literal(Constant(tpe + ": " + getters)))
  }

  def round[T: c.WeakTypeTag](world: c.Expr[Playground]): c.Expr[Unit] = {
    val rounding = for (f <- permanentFields) yield {
      def r(f: TermSymbol) = {
        val set = fieldSetter(f)
        val get = fieldGetter(f)
        val name = get.name.toString
        val min = Select(world.tree, TermName(name+"Min"))
        val max = Select(world.tree, TermName(name+"Max"))
        val step = Select(world.tree, TermName(name+"Discretization"))
        //c.echo(world.tree.pos, "rounding " + name + " in " + f.owner)
        q"$set(react.verification.Stateful.round($get, $min, $max, $step))"
      }
      fieldGetter(f).name.toString match {
        case "x" | "y" if f.typeSignature =:= definitions.DoubleTpe  => r(f)
        case _ =>
          //c.echo(world.tree.pos, "ignoring " + f + " in " + f.owner)
          q"()"
      }
    }
    val tree = q"{ ..$rounding }"
    c.Expr[Unit](tree)
  }
  
  def makeStateful[M: c.WeakTypeTag](robot: c.Expr[M], world: c.Expr[Playground]): c.Expr[Stateful] = {

    val caches = for (f <- permanentFields if !isNative(f) ) yield {
      val cid = cacheId(f)
      val t = f.typeSignature
      q"val $cid = new react.verification.Cache[$t](3)"
    }

    var length = wordLength[M]
    
    val storing = for (f <- permanentFields) yield {
      val getter = fieldGetter(f)
      write(f, c.Expr[ByteBuffer](Ident(TermName("out"))), getter)
    }

    val restored = for (f <- permanentFields) yield {
      val setter = fieldSetter(f)
      read(f, c.Expr[ByteBuffer](Ident(TermName("in"))), setter)
    }

    val havoced = for (f <- transientFields) yield {
      val setter = fieldSetter(f)
      havoc(f, setter)
    }
    
    val rounding = for (f <- permanentFields) yield {
      def r(f: TermSymbol) = {
        val set = fieldSetter(f)
        val get = fieldGetter(f)
        val name = get.name.toString
        val min = Select(world.tree, TermName(name+"Min"))
        val max = Select(world.tree, TermName(name+"Max"))
        val step = Select(world.tree, TermName(name+"Discretization"))
        //c.echo(world.tree.pos, "rounding " + name + " in " + f.owner)
        q"$set(react.verification.Stateful.round($get, $min, $max, $step))"
      }
      fieldGetter(f).name.toString match {
        case "x" | "y" if f.typeSignature =:= definitions.DoubleTpe  => r(f) //TODO somthing more robust
        case _ =>
          //c.echo(world.tree.pos, "ignoring " + f + " in " + f.owner)
          q"()"
      }
    }
    
    val descr = fieldsSaved[M]
    val tree = q"""
    new Stateful {
      import Stateful._
      val robot = $robot
      ..$caches
      def length: Int = $length
      def round: Unit = {
        ..$rounding
      }
      def serialize(out: java.nio.ByteBuffer): Unit = {
        ..$storing
      }
      def deserilize(in: java.nio.ByteBuffer): Unit = {
        ..$restored
        ..$havoced
      }
      def description: String = $descr
    }
    """
    c.Expr[Stateful](tree)
  }

}
