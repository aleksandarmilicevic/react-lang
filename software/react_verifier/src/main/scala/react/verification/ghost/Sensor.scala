package react.verification.ghost

import react._
import react.message._
import react.verification.environment._
import react.verification.model._
import react.verification._
import react.utils.RosUtils

//sensor are considered as stateless by the model checker (i.e. not saved/restored)
abstract class Sensor(parent: react.verification.model.GroundRobot,
                      _topic: String,
                      rate: Double) extends Executed {

  def topic = RosUtils.mayAddPrefix(parent.robotId, _topic)

  //period
  val period = math.round(1000.0 / rate).toInt

  //the robot carring the sensor is responsible for updating the pose of the sensor when it moves
  //this pose in defined in the map frame
  var pose: Pose2D = null

  protected var world: List[Box2D] = Nil

  //this method will be called each time the world changes
  def update(restOfTheWorld: List[Box2D]) {
    world = restOfTheWorld
  }

  //produce a value
  def act: Unit

  val task = new react.runtime.ScheduledTask(period, () => act )

  override def register(e: Executor) {
    super.register(e)
    exec.schedule(task)
  }

  override def deregister(e: Executor) {
    super.deregister(e)
    task.cancel
  }
}
