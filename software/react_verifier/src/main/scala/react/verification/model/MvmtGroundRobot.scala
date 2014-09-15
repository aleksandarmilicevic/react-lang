package react.verification.model

import react._
import react.robot._
import react.message._
import react.verification.environment._
import react.verification.ghost._
import react.verification._
import math._


/** Model for an ideal (execute command perfectly) robot moving on the ground */
class MvmtGroundRobot( bBox: Box2D,
                       _topic: String,
                       cmdTime: Int,
                       snap: Option[(String,String)] = None
                     ) extends GroundRobot(bBox, _topic, snap) {

  /* what to execute */
  var commandTimeLeft = 0
  var cmdQueue = List[(Int, Double, Double)]()
  
  override def elapse(t: Int) {

    var left = t
    while (left > 0) {
      if (commandTimeLeft == 0 && !cmdQueue.isEmpty) {
        val (t, s, a) = cmdQueue.head
        commandTimeLeft = t
        vx = s
        vo = a
        cmdQueue = cmdQueue.tail
      }

      val mt = min(left, commandTimeLeft)
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

      commandTimeLeft -= mt
      left -= mt
    }
    
    super.elapse(t)
     
    if (commandTimeLeft <= 0) {
      vx = 0.0
      vo = 0.0
    }

  }

  val listener = new org.ros.message.MessageListener[react_msgs.Mvmt]{
    def onNewMessage(message: react_msgs.Mvmt) {
      lock.lock
      try {
        vx = message.getSpeed
        vo = message.getAngularSpeed
        val d = message.getD
        commandTimeLeft = d.secs * 1000 + d.nsecs / 1000
      } finally lock.unlock
    }
  }

  override def register(exec: Executor) {
    super.register(exec)
    val sub = exec.getSubscriber[react_msgs.Mvmt](topic, react_msgs.Mvmt._TYPE)
    sub.addMessageListener(listener)
  }

  override def deregister(exec: Executor) {
    super.deregister(exec)
  }

}
