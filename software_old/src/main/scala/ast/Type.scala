package react.ast

import scala.util.parsing.input.Positional

sealed abstract class Type extends Positional
case object TAny extends Type
case object TBool extends Type
case object TInt extends Type
case object TFloat extends Type
case object TString extends Type
case class TArray(cnt: Type) extends Type //TODO or field
case class TTuple(elts: List[Type]) extends Type //TODO record
case class TFun(args: List[Type], ret: List[Type]) extends Type
case class TObj(name: Id) extends Type
//TODO record ? (and tuples as syntactic sugar for records with idx for members)
//react specific types: events, machines, ...


trait Typed {
  var tpe: Type = TAny
  def setType(t: Type): this.type = {
    tpe = t
    this
  }
}

