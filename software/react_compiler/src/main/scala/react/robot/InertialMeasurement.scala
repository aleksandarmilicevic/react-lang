package react.robot

import react._
import react.message._

//Some util methods for IMU

//http://docs.ros.org/api/sensor_msgs/html/msg/Imu.html

object InertialMeasurement {

  def hasOrientation(msg: Imu) = msg.orientationCovariance(0) != -1

  def hasLinearAcceleration(msg: Imu) = msg.linearAccelerationCovariance(0) != -1

  def hasAngularVelocity(msg: Imu) = msg.angularVelocityCovariance(0) != -1

}
