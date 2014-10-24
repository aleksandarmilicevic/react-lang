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
                a: Double = 650,
                b: Double = 10,
                c: Double = 0.5,
                d: Short = 5 
              ) extends Sensor(parent, _topic, std_msgs.Int16._TYPE, rate)
{

  protected def reponseFct(angle: Double, dist: Double): Short = {
    val expected = a / (b * dist + c)
    val faded = expected * fadeOut(angle)
    val bounded = max(min(a, faded), 0)
    val actual = (bounded + d).toShort
    actual
  }

  //good range is [-0.15,0.15] rad
  //then we fade to 0 on another 0.1 rad
  protected def fadeOut(angle: Double): Double = {
    val a = angle.abs - 0.15
    if (a <= 0) 1.0
    else if (a <= 0.1) (0.1 - a) * 10.0
    else 0.0
  }

  def act {

    var range: Short = d
    for (i <- 0 until 50) {
      val a = pose.theta -0.25 + i * 0.1
      val intersections = world.flatMap(_.intersectLine((pose.x, pose.y), (cos(a), sin(a))))
      val pos = intersections.filter(_ > 0)
      range = if (!pos.isEmpty) max(range, reponseFct(a, pos.min)).toShort else range
    }

    val msg = Primitive.Int16( range )
    val msg2 = exec.convertMessage[std_msgs.Int16](msg)
    exec.publish(topic, std_msgs.Int16._TYPE, msg2)
  }
}
