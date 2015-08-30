package react.verification

import scala.language.experimental.macros

import react.rewriting.ExplorableMacros
  
import java.nio.ByteBuffer

import scala.annotation.meta._
@field class ignore extends scala.annotation.StaticAnnotation

abstract class StateModifierIterator extends Iterator[Unit] { //of type unit because it work inplace
  def hasNext: Boolean
  def next: Unit
  def reset: Unit
}

/** a wrapper around generic objects that needs to be saved (and restored) by the model checker */
trait Stateful {
  def length: Int //in byte
  
  //discretization
  def nbrOfRoundedVars: Int
  def round: Unit
  def concretize: StateModifierIterator

  def serialize(out: ByteBuffer): Unit
  def deserilize(in: ByteBuffer): Unit

  def description: String
  def longDescription: String
  def worldAgnostic: Boolean
}

object Stateful {

  def makeStateful[M](robot: M, world: Playground): Stateful = macro ExplorableMacros.makeStateful[M]

  /** return a value ∈ [min,max] which is a multiple of step (or min, max) */
  def round(value: Double, min: Double, max: Double, step: Double): Double = {
    val stepped = math.rint(value / step) * step
    math.min(max, math.max(min, stepped))
  }

  /** given a rounded value returns the lower bound of the interval in which the value belongs */
  def lower(value: Double, min: Double, max: Double, step: Double): Double = {
    val floored = ((value / step) - 0.5) * step
    val res = math.max(min, floored + step * 1e-6)
    //assert(round(res, min, max, step) == value, "lower: " + value + " -> " + res + " -> " + round(res, min, max, step))
    res
  }

  /** given a rounded value returns the upper bound of the interval in which the value belongs */
  def upper(value: Double, min: Double, max: Double, step: Double): Double = {
    val ceiled = ((value / step) + 0.5) * step
    val res = math.min(max, ceiled - step * 1e-6)
    //assert(round(res, min, max, step) == value, "upper: " + value + " -> " + res + " -> " + round(res, min, max, step))
    res
  }

  /** given a rounded value returns the interval in which the value is */
  def interval(value: Double, min: Double, max: Double, step: Double): (Double, Double) = {
    (lower(value, min, max, step), upper(value, min, max, step))
  }

}

