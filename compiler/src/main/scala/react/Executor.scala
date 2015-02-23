package react

import react.message._

/** An common interface to execute robot or verify them */
trait Executor {

  def publish[T](topic: String, typeName: String, message: T)

//def delayedPublish[T](delay: Int, topic: String, typeName: String, message: T)
  
  def getSubscriber[T](topic: String, typeName: String): org.ros.node.topic.Subscriber[T]
  def messageDelivered: Unit = {}
  
  /* conversion between REACT and ROS messages */
  def convertMessage[N](msg: Message): N

  def schedule(t: react.runtime.ScheduledTask)

  def removeCanceledTask: Unit

}
