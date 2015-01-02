package react.verification.model

import react.robot._
import react.utils._
import react.message._
import react.runtime.MessageListenerRW
import react.Executor
import math._

class Servo[A <: Positioned](
    topic: String
  ) extends FrameTransformer[A] {

  //angle ∈ [-π/2,π/2]
  var angle = 0.0

  override protected def offsetTransform(p: Pose2D): Pose2D = {
    val cx = cos(angle) * p.x - sin(angle) * p.y
    val cy = sin(angle) * p.x + cos(angle) * p.y
    Pose2D(cx, cy, p.theta)
  }

  override def register(exec: Executor) {
    super.register(exec)
    val listener = new MessageListenerRW[std_msgs.Int16]{
      def robotID = "servo("+topic+")"
      override def read = Some(Set())
      override def written = Some(Set("angle"))
      override def sendMsgsTo = Some(Set())
      def onNewMessage(message: std_msgs.Int16) {
        angle = (message.getData - 90) * Pi / 180.0
        Logger("Servo", LogDebug, robotID + " -> " + message.getData + " -> " + angle)
        exec.messageDelivered
      }
    }
    val sub = exec.getSubscriber[std_msgs.Int16](topic, std_msgs.Int16._TYPE)
    sub.addMessageListener(listener)
  }

}
