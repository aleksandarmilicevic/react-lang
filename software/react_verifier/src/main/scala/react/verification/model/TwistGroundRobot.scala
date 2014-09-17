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
    val dt = mt / 1000.0

    if (vo == 0.0) {
      x += dt * vx * cos(orientation)
      y += dt * vx * sin(orientation)
    } else {
      val r = vx / vo
      val dx = r * cos(vo*dt)
      val dy = r * sin(vo*dt)
      x += dx * cos(orientation) - dy * sin(orientation)
      y += dx * sin(orientation) + dy * cos(orientation)
    }
    orientation += vo * dt

    super.elapse(t)

    commandTimeLeft -= mt
    if (commandTimeLeft <= 0) {
      vx = 0.0
      vo = 0.0
    }
  //println("x = " + x)
  //println("y = " + y)
  //println("Θ = " + orientation)
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
