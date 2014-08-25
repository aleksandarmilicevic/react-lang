package react.verification.ghost

import react.message._
import react.verification.environment._
import react.verification.model._
import react.verification._
import react.utils.RosUtils

//sensor are considered as stateless by the model checker (i.e. not saved/restored)
abstract class Sensor(parent: TwistGroundRobot,
                      _topic: String,
                      rate: Double) extends Executed {

  def topic = RosUtils.mayAddPrefix(parent.robotId, _topic)

  //period
  val period = math.round(1000.0 / rate).toInt

  //the robot carring the sensor is responsible for updating the pose of the sensor when it moves
  //this pose in defined in the map frame
  var pose: Pose2D = null

  //time ∈ [0,period)
  var t = 0
  
  //when will this sensor publish its next message
  def nextActionIn = period - (t % period)

  protected var world: List[Box2D] = Nil

  //this method will be called each time the world changes
  def update(restOfTheWorld: List[Box2D]) {
    world = restOfTheWorld
  }

  //produce a value
  def act: Unit

  def elapse(d: Int): Unit = {
    var left = d
    while (left > 0) {
      val delta = math.min(left, period)
      t += delta
      left -= delta
      if(t >= period) {
        act
      }
      t = t % period
    }
  }

}
