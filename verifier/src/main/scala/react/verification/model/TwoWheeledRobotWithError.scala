package react.verification.model

import react._
import react.robot._
import react.message._
import react.runtime.MessageListenerRW
import react.verification.environment._
import react.verification.ghost._
import react.verification._
import react.verification.modelchecker.BranchingPoint
import math._
import react.utils._

/** TwoWheeledRobot but with error/imprecision on the motors */
class TwoWheeledRobotWithError(
                       bBox: Box2D,
                       port: String,
                       leftWheelPin: String,
                       rightWheelPin: String,
                       wheelRadius: Double,
                       wheelSpacing: Double,
                       relativeErr: Double,
                       absoluteErr: Short
                     ) extends TwoWheeledRobot(bBox, port, leftWheelPin, rightWheelPin, wheelRadius, wheelSpacing)
{

  override def elapseBP(t: Int): BranchingPoint = {
    new BranchingPoint {
      def alternatives = 9
  
      def act(alt: Int): List[String] = {
        //save the actual motor values
        val l = vl
        val r = vr
        //error on the motor
        val el = (vl.abs * relativeErr + absoluteErr).toShort
        val er = (vr.abs * relativeErr + absoluteErr).toShort
        //
        val label = alt match {
          case 1 => vr = (vr + er).toShort; "0+"
          case 2 => vr = (vr - er).toShort; "0-"
          case 3 => vl = (vl + el).toShort; "+0"
          case 4 => vl = (vl - el).toShort; "-0"
          case 5 => vl = (vl + el).toShort; vr = (vr + er).toShort; "++"
          case 6 => vl = (vl + el).toShort; vr = (vr - er).toShort; "+-"
          case 7 => vl = (vl - el).toShort; vr = (vr + er).toShort; "-+"
          case 8 => vl = (vl - el).toShort; vr = (vr - er).toShort; "--"
          case _ => "00"
        }
        //
        elapse(t)
        // restore the original values
        vl = l
        vr = r
        List("elapse"+label+"("+t+")")
      }
    }
  }
}
