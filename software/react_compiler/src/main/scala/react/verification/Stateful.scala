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
  def longDescription: String
}

object Stateful {

  def makeStateful[M](robot: M, world: Playground): Stateful = macro ExplorableMacros.makeStateful[M]

  def round(value: Double, min: Double, max: Double, step: Double): Double = {
    val stepped = math.rint(value / step) * step
    val res = math.min(max, math.max(min, stepped))
    //println("rounding " + value + " to " + res + " ("+min+", "+max+","+step+")")
    res
  }

}

