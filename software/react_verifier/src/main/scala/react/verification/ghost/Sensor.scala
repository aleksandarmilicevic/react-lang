package react.verification.ghost

import react.message._

//TODO sensor have a position relative to some part of the robot ...

abstract class Sensor(topic: String,
                      rate: Double
                      ) extends Ghost {

  //the robot carring the sensor is responsible for updating the pose of the sensor when it moves
  var pose: Pose2D = null
  
  //when can this ghost make something ?
  override def nextActionIn = -1

  //period of the events
  override def period = math.round(1000.0 / rate).toInt

}
