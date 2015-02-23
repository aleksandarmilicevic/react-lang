package react.verification.ghost

import react.message._
import react.verification.model._
import react.verification.Stateful
import math._

//this is horizontal only
class LaserSensor(minAngle: Float,
                  maxAngle: Float,
                  samples: Int,
                  minRange: Float,
                  maxRange: Float,
                  resolution: Float,
                  parent: GroundRobot,
                  _topic: String,
                  rate: Double
                 ) extends Sensor(parent, _topic, sensor_msgs.LaserScan._TYPE, rate) {

  val frameName = "GhostLaserSensor" //TODO user defined name
  val dummyHeader = Header(0, Time(0, 0), frameName)

  def act {
    //println(frameName + ": " + pose + ", |world| = " + world.size)
    val rays = Array.ofDim[Float](samples)
    val intensities = Array.ofDim[Float](samples)

    val da = (maxAngle - minAngle) / samples
    for(i <- 0 until samples) {
      val a = pose.theta + minAngle + i * da
      val intersections = world.flatMap(_.intersectLine((pose.x, pose.y), (cos(a), sin(a))))
      val pos = intersections.filter(_ > 0)
      val closest = if (!pos.isEmpty) pos.min else maxRange
      val r = Stateful.round(closest, minRange, maxRange, resolution).toFloat
      rays(i) = r
      intensities(i) = 1.0f / r / r
    }

    val msg = LaserScan(
      dummyHeader,
      minAngle,
      maxAngle,
      da,
      0,
      period,
      minRange,
      maxRange,
      rays,
      intensities
    )

    val msg2 = exec.convertMessage[sensor_msgs.LaserScan](msg)

    exec.publish(topic, sensor_msgs.LaserScan._TYPE, msg2)
  }

}
