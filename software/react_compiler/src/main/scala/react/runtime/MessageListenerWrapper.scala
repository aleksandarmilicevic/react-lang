package react.runtime

import react._
import react.message._


abstract class MessageListenerWrapper {

//import org.ros.message.MessageListener
//import java.util.concurrent.locks.ReentrantLock

//val topic: String
//val rosType: String
//def handler(msg: M): Unit
//val lock: ReentrantLock

//val listener = new MessageListener[M]{
//  def onNewMessage(message: M) {
//    lock.lock()
//    try {
//      handler(message)
//    } finally {
//      lock.unlock
//    }
//  }
//}

  var registered = false
  def register(exec: RobotExecutor): Unit
//{
//  if (!registered) {
//    registered = true
//    val sub = exec.getSubscriber[M](topic, rosType)
//    sub.addMessageListener(listener)
//  }
//}
  
  var enabled = false

  def enable { enabled = true }

  def disable { enabled = false }

}
