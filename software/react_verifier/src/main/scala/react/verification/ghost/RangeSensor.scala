package react.verification.ghost

import react.message._
import react.verification.model._
import react.verification.Stateful
import math._

class RangeSensor(kind: Byte,
                  fov: Float,
                  minRange: Float,
                  maxRange: Float,
                  resolution: Float,
                  parent: GroundRobot,
                  _topic: String,
                  rate: Double
                 ) extends Sensor(parent, _topic, sensor_msgs.Range._TYPE, rate)
{

  val frameName = "GhostRangeSensor" //TODO user defined name
  val dummyHeader = Header(0, Time(0, 0), frameName)

  def inFOV(p: (Double, Double)) = {
    val x = p._1
    val y = p._2
    val a = atan2(y, x) - pose.theta
    val f2 = fov / 2
    a >= -f2 && a <= -f2
  }

  def act {

    //take into acount the FOV
    //TODO to make this correct we should "restrict" the obstacle to the FOV
    val inView = world.filter( b => {
      b.corners.exists(inFOV) || inFOV(b.center)
    })

    val dist = inView.foldLeft(maxRange: Double)( (acc, b) => min(acc, b.distance(pose.x, pose.y)) )
    val range = max(minRange, dist)
    val range2 = Stateful.round(range, minRange, maxRange, resolution).toFloat

    val msg = Range(
      dummyHeader,
      kind,
      fov,
      minRange,
      maxRange,
      range2
    )

    val msg2 = exec.convertMessage[sensor_msgs.Range](msg)

    exec.publish(topic, sensor_msgs.Range._TYPE, msg2)
  }
}
