package react.verification.modelchecker

import org.ros.node.topic._
import org.ros.message.MessageListener
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.TimeUnit

class SubscriberWithBypass[T](sub: Subscriber[T]) extends Subscriber[T] {
  import org.ros.node.topic._
  val lock = new ReentrantLock()
  var cnt = 0
  var listeners: List[MessageListener[T]] = Nil
  def addMessageListener(listener: MessageListener[T], limit: Int) {
    lock.lock
    try {
      sub.addMessageListener(listener, limit)
      listeners = listener :: listeners
      cnt += 1
    } finally {
      lock.unlock
    }
  }
  def addMessageListener(listener: MessageListener[T]) {
    lock.lock
    try {
      sub.addMessageListener(listener)
      listeners = listener :: listeners
      cnt += 1
    } finally {
      lock.unlock
    }
  }
  def getLatchMode = sub.getLatchMode
  def addSubscriberListener(listener: SubscriberListener[T]) {
    sub.addSubscriberListener(listener)
  }
  def shutdown = sub.shutdown
  def shutdown(timeout: Long, unit: TimeUnit) = sub.shutdown(timeout, unit) 
  def getTopicMessageType = sub.getTopicMessageType
  def getTopicName = sub.getTopicName
}
