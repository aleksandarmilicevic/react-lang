package react.message

//some of the ros std_msgs
object Primitive {
  case object Empty extends Message(std_msgs.Empty._TYPE)
  case class Bool(data: Boolean) extends Message(std_msgs.Bool._TYPE)
  case class Byte(data: scala.Byte) extends Message(std_msgs.Byte._TYPE)
  case class Char(data: scala.Char) extends Message(std_msgs.Char._TYPE)
  case class Float32(data: Float) extends Message(std_msgs.Float32._TYPE)
  case class Float64(data: Double) extends Message(std_msgs.Float64._TYPE)
  case class Int16(data: Short) extends Message(std_msgs.Int16._TYPE)
  case class Int32(data: Int) extends Message(std_msgs.Int32._TYPE)
  case class Int64(data: Long) extends Message(std_msgs.Int64._TYPE)
  case class String(data: java.lang.String) extends Message(std_msgs.String._TYPE)
  case class Duration(data: Long) extends Message(std_msgs.Duration._TYPE)
  case class Time(data: Long) extends Message(std_msgs.Time._TYPE)

  def is(rosTpe: java.lang.String) = {
    rosTpe == std_msgs.Empty._TYPE ||
    rosTpe == std_msgs.Bool._TYPE ||
    rosTpe == std_msgs.Byte._TYPE ||
    rosTpe == std_msgs.Char._TYPE ||
    rosTpe == std_msgs.Int16._TYPE ||
    rosTpe == std_msgs.Int32._TYPE ||
    rosTpe == std_msgs.Int64._TYPE ||
    rosTpe == std_msgs.Float32._TYPE ||
    rosTpe == std_msgs.Float64._TYPE ||
    rosTpe == std_msgs.String._TYPE ||
    rosTpe == std_msgs.Duration._TYPE ||
    rosTpe == std_msgs.Time._TYPE
  }

}

object PrimitiveConversion {
  implicit def primitiveToBool(data: Boolean) = Primitive.Bool(data)
  implicit def primitiveToByte(data: scala.Byte) = Primitive.Byte(data)
  implicit def primitiveToChar(data: scala.Char) = Primitive.Char(data)
  implicit def primitiveToFloat32(data: Float) = Primitive.Float32(data)
  implicit def primitiveToFloat64(data: Double) = Primitive.Float64(data)
  implicit def primitiveToInt16(data: Short) = Primitive.Int16(data)
  implicit def primitiveToInt32(data: Int) = Primitive.Int32(data)
  implicit def primitiveToInt64(data: Long) = Primitive.Int64(data)
}
