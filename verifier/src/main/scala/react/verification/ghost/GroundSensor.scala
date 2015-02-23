package react.verification.ghost

import react.message._
import react.verification.model._
import react.verification.environment._

/** This emulates sensors that recognize things on the ground.
 *  For instance it can be detect patches of color to "follow the line",
 *  find a target or even work as cliff sensors.
 */
class GroundSensor( targets: Iterable[Box2D],
                    parent: GroundRobot,
                    _topic: String,
                    rate: Double,
                    inverted: Boolean = false
                  ) extends Sensor(parent, _topic, std_msgs.Bool._TYPE, rate)
{

  def act {
    val x = pose.x
    val y = pose.y
    val contained = targets.exists(_.contains(x, y))
    val res = if (inverted) !contained else contained
    val msg = Primitive.Bool( res )
    val msg2 = exec.convertMessage[std_msgs.Bool](msg)
    exec.publish(topic, std_msgs.Bool._TYPE, msg2)
  }

}
