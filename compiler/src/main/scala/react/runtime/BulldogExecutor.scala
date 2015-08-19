package react.runtime

import react._
import react.message._
import react.runtime.bulldog._
import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.ReentrantLock
import org.ros.node.NodeConfiguration
import org.ros.message.MessageFactory
import io.silverspoon.bulldog.core.platform.{Platform,Board}

//TODO could we have an hybrid that works with both ROS and libBulldog at the same time ?
//     if the topic is a Bulldog name then it uses libBulldog, otherwise ROS ?

//TODO define an hardware mapping
//  generalization of debounceMS, periodMS, threshold
//  given some pin identifier, we should know what to use, e.g., servo to PWM
//  also some topic could correspong to multiple pins, like uart (bytes or string)

class BulldogExecutor(val robot: Robot, val hal: HAL) extends Executor {

  protected val scheduler = new Scheduler
  
  def getPublisher[T](topic: String, typeName: String): org.ros.node.topic.Publisher[T] = {
    hal.getPublisher[T](topic, typeName)
  }
  
  def publish[T](topic: String, typeName: String, message: T) = {
    val pub = getPublisher[T](topic, typeName)
    pub.publish(message)
  }

  
  def getSubscriber[T](topic: String, typeName: String): org.ros.node.topic.Subscriber[T] = {
    hal.getSubscriber[T](topic, typeName)
  }
  
  private val messageFactory = NodeConfiguration.newPrivate().getTopicMessageFactory()
  /* conversion between REACT and ROS messages */
  def convertMessage[N](msg: Message): N = {
    Message.toMessage(messageFactory, msg).asInstanceOf[N] //TODO something nicer to avoid the casting
  }

  def schedule(t: react.runtime.ScheduledTask) = scheduler.schedule(t)

  def removeCanceledTask: Unit = scheduler.removeCanceled
  
  protected def initialize {
    hal.initialize(messageFactory)
  }
  
  //the big while loop
  def start {
    initialize
    robot.setExec(this)
    while (true) {
      scheduler.waitUntilNextTask match {
        case Some(task) =>
          robot.lock.lock
          try {
            task.fct()
          } finally {
            robot.lock.unlock
          }
          if (task.isPeriodic && !task.cancelled) {
            scheduler.schedule(task)
          }
        case None => Thread.sleep(1)
      }
    }
  }
  
  def shutdown {
    robot.deregister(this)
    hal.shutdown
  }

}
