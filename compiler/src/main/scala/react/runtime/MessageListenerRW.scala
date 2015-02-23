package react.runtime

import org.ros.message.MessageListener

trait RW {
  def robotID: String
  def read: Option[Set[String]] = None
  def written: Option[Set[String]] = None
  def sendMsgsTo: Option[Set[(String,String)]] = None
}

abstract class MessageListenerRW[M] extends MessageListener[M] with RW {
}

