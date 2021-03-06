package react.verification.model

import react._
import react.robot._
import react.message._
import react.runtime.MessageListenerRW
import react.verification.environment._
import react.verification.ghost._
import react.verification._
import math._


/** Model for an ideal (execute command perfectly) robot moving on the ground */
class MvmtGroundRobot( bBox: Box2D,
                       val topic: String,
                       cmdTime: Int,
                       snap: Option[(String,String)] = None
                     ) extends GroundRobot(bBox, snap) {

  /* what to execute */
  var commandTimeLeft: Short = 0
  var cmdQueue = List[(Short, Double, Double)]()
  
  override def elapse(t: Int) {

    var left = t
    while (left > 0) {
      if (commandTimeLeft == 0) {
        if (!cmdQueue.isEmpty) {
          val (t, s, a) = cmdQueue.head
          commandTimeLeft = t
          vx = s
          vo = a
          cmdQueue = cmdQueue.tail
        } else {
          commandTimeLeft = left.toShort
          assert(commandTimeLeft >= 0)
          vx = 0.0
          vo = 0.0
        }
      }
      val mt = min(left, commandTimeLeft)
      super.elapse(mt)
      commandTimeLeft = (commandTimeLeft - mt).toShort
      left -= mt
    }
     
    if (commandTimeLeft <= 0) {
      vx = 0.0
      vo = 0.0
      super.elapse(0)
    }

  }


  override def register(exec: Executor) {
    super.register(exec)
    val listener = new MessageListenerRW[react_msgs.Mvmt]{
      def robotID = MvmtGroundRobot.this.toString //TODO better
      override def read = Some(Set())
      override def written = Some(Set("vx" , "vo", "commandTimeLeft"))
      def onNewMessage(message: react_msgs.Mvmt) {
        lock.lock
        try {
          vx = message.getSpeed
          vo = message.getAngularSpeed
          val d = message.getD
          commandTimeLeft = (d.secs * 1000 + d.nsecs / 1000).toShort
          assert(commandTimeLeft >= 0)
        } finally lock.unlock
        exec.messageDelivered
      }
    }
    val sub = exec.getSubscriber[react_msgs.Mvmt](topic, react_msgs.Mvmt._TYPE)
    sub.addMessageListener(listener)
  }

  override def deregister(exec: Executor) {
    super.deregister(exec)
  }

}
