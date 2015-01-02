package react.runtime

import react._
import react.message._

abstract class MessageListenerWrapper {

  var registered = false
  def register(exec: Executor): Unit
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
