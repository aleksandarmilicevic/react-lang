package react.examples.arduino

import react.Robot
import react.message.Primitive.Int16

class Arm(port: String) extends Robot(port) {

  val range = 30
  val step  =  5
  var a = 0
  var b = 0
  var c = 0

  every(4 * range * range / step / step) {
    a += step
    if (a > range) {
      a = -range
    }
    publish("alpha", Int16(a.toShort))
  }

  every(2 * range / step) {
    b += step
    if (b > range) {
      b = -range
    }
    publish("beta", Int16(b.toShort))
  }
  
  every(1) {
    c += step
    if (c > range) {
      c = -range
    }
    publish("gamma", Int16(c.toShort))
  }

}
