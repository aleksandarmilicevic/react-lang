package react.examples.bbb

import react.Robot
import react.robot._
import react.runtime._
import react.runtime.bulldog._
import react.message.Primitive.Float64
import org.ros.message.MessageFactory
import io.silverspoon.bulldog.beagleboneblack.BBBNames

class Arm(port: String) extends Robot(port) {

  val alpha = BBBNames.PWM_P9_16
  val beta  = BBBNames.PWM_P8_13

  var counter = 0
  val period = 2000

  every(period) {
    counter match {
      case 0 =>
        counter += 1
        publish(alpha, Float64(-21))
        publish(beta,  Float64(-18))
      case 1 =>
        counter += 1
        publish(alpha, Float64(-30))
        publish(beta,  Float64( 60))
      case 2 =>
        counter += 1
        publish(alpha, Float64( 39))
        publish(beta,  Float64(-17))
      case _ =>
        counter = 0
    }
  }

}

class ArmConfig extends HAL {

  override def initialize(m: MessageFactory) {
    super.initialize(m)
    publishers += (BBBNames.PWM_P9_16 -> new SmoothServoPublisher(board, m, BBBNames.PWM_P9_16))
    publishers += (BBBNames.PWM_P8_13 -> new SmoothServoPublisher(board, m, BBBNames.PWM_P8_13))
  }

}
