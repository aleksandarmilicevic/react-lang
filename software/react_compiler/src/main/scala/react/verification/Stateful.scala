package react.verification

import scala.language.experimental.macros

import react.rewriting.ExplorableMacros
  
import java.nio.ByteBuffer

import scala.annotation.meta._
@field class ignore extends scala.annotation.StaticAnnotation

/** a wrapper around generic objects that needs to be saved (and restored) by the model checker */
trait Stateful {
  def length: Int //in byte
  def round: Unit
  def serialize(out: ByteBuffer): Unit
  def deserilize(in: ByteBuffer): Unit
  def description: String
}

object Stateful {

  implicit class Explorable[M](val robot: M) extends AnyVal {
    def length: Int = macro ExplorableMacros.wordLength[M] //in byte
    def round(world: Playground): Unit = macro ExplorableMacros.round[M]
    def serialize(out: ByteBuffer): Unit = macro ExplorableMacros.toWord[M]
    def deserilize(in: ByteBuffer): Unit = macro ExplorableMacros.fromWord[M]
    def description: String = macro ExplorableMacros.fieldsSaved[M]
  }

  def round(value: Double, min: Double, max: Double, step: Double): Double = {
    val stepped = (value / step).round * step
    val res = math.min(max, math.max(min, stepped))
    //Console.println("rounding " + value + " to " + res)
    res
  }

}

