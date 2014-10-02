package react.runtime

import org.ros.message.MessageListener

abstract class MessageListenerRW[M] extends MessageListener[M] {
  def read: Option[List[String]] = None
  def written: Option[List[String]] = None
}

