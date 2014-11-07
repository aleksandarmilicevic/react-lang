package react.verification.ghost

import react.message._
import react.verification.model._
import math._


/** The goal of this class is to provide a reasonable model
 *  of an IR range sensor (such as the ones produced by Sharp)
 *  connected to an Arduino analog input pin.
 *  We assume that the sensor is mounted *verticaly*
 */
class IrSensor( parent: GroundRobot,
                _topic: String,
                rate: Double,
                beamAngle: Double = 0.5,
                additionalFading: Double = 0.2,
                a: Double = 650,
                b: Double = 10,
                c: Double = 0.5,
                d: Short = 5 
              ) extends Sensor(parent, _topic, std_msgs.Int16._TYPE, rate)
{

  val ba = beamAngle/2
  val fa = additionalFading/2
  val minAngle = -(ba+fa)
  val maxAngle =  (ba+fa)
  val steps = 50
  val step = (maxAngle - minAngle)/steps


  protected def responseFct(angle: Double, dist: Double): Short = {
    val expected = a / (b * dist + c)
    val faded = expected * fadeOut(angle)
    val bounded = max(min(a, faded), 0)
    val actual = (bounded + d).toShort
    actual
  }

  //good range is [-0.15,0.15] rad
  //then we fade to 0 on another 0.1 rad
  protected def fadeOut(angle: Double): Double = {
    val a = angle.abs - (ba - fa)
    if (a <= 0) 1.0
    else if (a <= fa) (fa - a) / fa
    else 0.0
  }

  def act {

    var range: Short = d
    for (i <- 0 until steps) {
      val a1 = minAngle + i * step
      val a = pose.theta + a1
      val intersections = world.flatMap(_.intersectLine((pose.x, pose.y), (cos(a), sin(a))))
      val pos = intersections.filter(_ > 0)
      if (!pos.isEmpty) {
        val m = pos.min
        val resp = responseFct(a1, m)
      //println("x: " + pose.x +
      //      ", y: " + pose.y +
      //      ", o: " + pose.theta +
      //      ", a: " + a1 +
      //      ", dist: " + m +
      //      ", response: " + resp)
        range = max(range, resp).toShort
      }
    }

    val msg = Primitive.Int16( range )
    val msg2 = exec.convertMessage[std_msgs.Int16](msg)
    exec.publish(topic, std_msgs.Int16._TYPE, msg2)
  }
}
