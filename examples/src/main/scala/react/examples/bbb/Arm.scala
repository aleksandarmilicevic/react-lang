package react.examples.bbb

import react.Robot
import react.robot._
import react.runtime._
import react.runtime.bulldog._
import react.message.Primitive.Float64
import react.message.PrimitiveConversion._
import org.ros.message.MessageFactory
import io.silverspoon.bulldog.beagleboneblack.BBBNames
import ArmConfig.{l0,l1}

class Arm(port: String) extends Robot(port) {

  var counter = 0

  every( 2000 ) {
    counter match {
      case 0 =>
        counter += 1
        publish[Float64](l0, -21)
        publish[Float64](l1, -18)
      case 1 =>
        counter += 1
        publish[Float64](l0, -30)
        publish[Float64](l1,  60)
      case 2 =>
        counter += 1
        publish[Float64](l0,  39)
        publish[Float64](l1, -17)
      case _ =>
        counter = 0
    }
  }

}

object ArmConfig {
  final val l0 = BBBNames.PWM_P9_16
  final val l1 = BBBNames.PWM_P8_13
}

class ArmConfig extends HAL {

  override def initialize(m: MessageFactory) {
    super.initialize(m)
    publishers += (l0 -> new SmoothServoPublisher(board, m, l0))
    publishers += (l1 -> new SmoothServoPublisher(board, m, l1))
  }

}
