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
                      msgType: String,
                      rate: Double) extends Positioned with Executed {

  def topic = RosUtils.mayAddPrefix(parent.robotId, _topic)

  //period
  val period = math.round(1000.0 / rate).toInt

  //the robot carring the sensor is responsible for updating the pose of the sensor when it moves
  //this pose in defined in the map frame
  var pose: Pose2D = null

  protected var world: List[Box2D] = Nil
  //TODO add other sensors so we can compute interferences

  //this method will be called each time the world changes
  def update(restOfTheWorld: List[Box2D]) {
    world = restOfTheWorld
  }

  //produce a value
  def act: Unit

  val id = "sensor["+msgType+"]("+_topic+")"
  val rw = new react.runtime.RW {
    def robotID = id
    override def read = Some(Set())
    override def written = Some(Set())
    override def sendMsgsTo = Some(Set(_topic -> msgType))
  }
  val task = new react.runtime.ScheduledTask( id,
                                              period,
                                              () => act,
                                              -1,
                                              false,
                                              Some(rw)
                                            )

  override def register(e: Executor) {
    super.register(e)
    exec.schedule(task)
  }

  override def deregister(e: Executor) {
    super.deregister(e)
    task.cancel
  }
}
