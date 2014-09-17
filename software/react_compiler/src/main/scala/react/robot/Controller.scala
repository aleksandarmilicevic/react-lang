package react.robot

import react._
import react.message._
import react.runtime._

import scala.language.experimental.macros
import react.rewriting.{RobotMacros, ExplorableMacros}

trait Controller {
    
  //////////////////
  // for the user //
  //////////////////

  val id: String
  val lock: java.util.concurrent.locks.ReentrantLock

  /** for non-ROS communication, e.g., message passing within the REACT program */
  def on(handler: PartialFunction[Any, Unit]) = addHandler(handler)
 
  /** a periodic controller (period in milliseconds)*/
  def every(period: Int)(body: Unit): Unit = macro RobotMacros.every
  
  /** subscribe to a topic using the REACT messages */
  def sensor[T <: Message](source: String)(handler: PartialFunction[T, Unit]): Unit = macro RobotMacros.registerHandler[T]
 
  /** subscribe to a topic directly using ROS messages */
  def subscribe[T](source: String, msgType: String)(handler: T => Unit) = {
    val wrapper = new react.runtime.MessageListenerWrapper{
      import org.ros.message.MessageListener
      val topic: String = source
      val rosType: String = msgType
      val listener = new MessageListener[T]{
        def onNewMessage(message: T) {
          if (enabled) {
            lock.lock()
            try {
              handler(message)
            } finally {
              lock.unlock
            }
          }
        }
      }
      def register(exec: Executor) {
        registered = true
        val sub = exec.getSubscriber[T](topic, rosType)
        sub.addMessageListener(listener)
      }
    }
    addSensor(wrapper)
  }
  
  ////////////////////
  // datastructures //
  ////////////////////

  protected var tasks: List[ScheduledTask] = Nil
  protected var handlers: List[PartialFunction[Any, Unit]] = Nil
  protected var sensors: List[MessageListenerWrapper] = Nil

  def addTask(task: ScheduledTask) {
    tasks = task :: tasks
  }
  def addHandler(handler: PartialFunction[Any, Unit]) {
    handlers = handler :: handlers
  }
  def addSensor(wrapper: MessageListenerWrapper) {
    sensors = wrapper :: sensors
  }
  /** return all tasks (used by verification to compute the period)*/
  def getAllTasks = tasks

    
  /////////////////////
  // for the runtime //
  /////////////////////

  //TODO get rid of that, everything should be ros message only
  def send(any: Any) {
    val defined = handlers.filter(_.isDefinedAt(any))
    lock.lock
    try {
      defined.foreach(_.apply(any))
    } finally {
      lock.unlock
    }
  }


  def register(exec: Executor) {
    for(t <- tasks) {
      t.expires = -1
      exec.schedule(t)
    }
    for(s <- sensors) {
      s.register(exec)
      s.enable
    }
  }

  def deregister(exec: Executor) {
    for(t <- tasks) {
      t.cancel
    }
    exec.removeCanceledTask
    for(s <- sensors) {
      s.disable
    }
  }

}
