package react.message

//sensor_msgs
case class Range(header: Header, kind: Byte, fov: Float, min: Float, max: Float, range: Float) extends Message(sensor_msgs.Range._TYPE)
case class LaserScan( header: Header,
                      angleMin: Float,
                      angleMax: Float,
                      angleIncrement: Float,
                      timeIncrement: Float,
                      scanTime: Float,
                      rangeMin: Float,
                      rangeMax: Float,
                      ranges: Array[Float],
                      intensities: Array[Float]) extends Message(sensor_msgs.LaserScan._TYPE)
case class Imu( header: Header,
                orientation: Quaternion,
                orientationCovariance: Array[Double], //9
                angularVelocity: Vector3,
                angularVelocityCovariance: Array[Double], //9
                linearAcceleration: Vector3,
                linearAccelerationCovariance: Array[Double] //9
            ) extends Message(sensor_msgs.Imu._TYPE) {
  assert(orientationCovariance.size == 9, "Imu.orientationCovariance must contain 9 elements")
  assert(angularVelocityCovariance.size == 9, "Imu.angularVelocityCovariance must contain 9 elements")
  assert(linearAccelerationCovariance.size == 9, "Imu.linearAccelerationCovariance must contain 9 elements")
}

