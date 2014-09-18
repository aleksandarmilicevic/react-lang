package react.verification.model

import react._
import react.robot._
import react.message._
import react.verification.environment._
import react.verification.ghost._
import react.verification._
import math._

/** Model for an ideal (execute command perfectly) robot moving on the ground */
class TwistGroundRobot( bBox: Box2D,
                        val topic: String,
                        cmdTime: Int,
                        snap: Option[(String,String)] = None
                      ) extends GroundRobot(bBox, snap) {

  /* what to execute */
  var commandTimeLeft = 0
  
  override def elapse(t: Int) {
    val mt = min(t, commandTimeLeft)

    super.elapse(mt)

    commandTimeLeft -= mt
    if (commandTimeLeft <= 0) {
      vx = 0.0
      vo = 0.0
    }
    
    if (mt < t) {
      super.elapse(t - mt)
    }
  }

  val listener = new org.ros.message.MessageListener[geometry_msgs.Twist]{
    def onNewMessage(message: geometry_msgs.Twist) {
      lock.lock
      try {
        commandTimeLeft = cmdTime
        vx = message.getLinear.getX
        vo = message.getAngular.getZ
      } finally lock.unlock
    }
  }

  override def register(exec: Executor) {
    super.register(exec)
    val sub = exec.getSubscriber[geometry_msgs.Twist](topic, geometry_msgs.Twist._TYPE)
    sub.addMessageListener(listener)
  }

  override def deregister(exec: Executor) {
    super.deregister(exec)
  }

}
