package react.verification

import scala.language.experimental.macros

import react.rewriting.ExplorableMacros
  
import java.nio.ByteBuffer

import scala.annotation.meta._
@field class ignore extends scala.annotation.StaticAnnotation

/** a wrapper around generic objects that needs to be saved (and restored) by the model checker */
trait Stateful {
  def length: Int //in byte
  def serialize(out: ByteBuffer): Unit
  def deserilize(in: ByteBuffer): Unit
  def description: String
}

object Stateful {

  implicit class Explorable[M](val robot: M) extends AnyVal {
    def length(world: Playground): Int = macro ExplorableMacros.wordLength[M] //in byte
    def serialize(world: Playground, out: ByteBuffer): Unit = macro ExplorableMacros.toWord[M]
    def deserilize(world: Playground, in: ByteBuffer): Unit = macro ExplorableMacros.fromWord[M]
    def description: String = macro ExplorableMacros.fieldsSaved[M]
  }

}

