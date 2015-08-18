package react.runtime

import react._
import react.message._
import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.ReentrantLock
import org.ros.node.NodeConfiguration
import org.ros.message.MessageFactory
import io.silverspoon.bulldog.core.platform.{Platform,Board}
import io.silverspoon.bulldog.core.gpio.{AnalogInput,DigitalInput}
import io.silverspoon.bulldog.core.gpio.event.ThresholdListener
import io.silverspoon.bulldog.core.Signal
import io.silverspoon.bulldog.core.util.BulldogUtil
import io.silverspoon.bulldog.devices.switches.{Button,ButtonListener}

//TODO could we have an hybrid that works with both ROS and libBulldog at the same time ?
//     if the topic is a Bulldog name then it uses libBulldog, otherwise ROS ?

class BulldogExecutor(val robot: Robot,
                      val debounceMS: Int = 0,
                      val periodMS: Int = 5,
                      val threshold: Double = 0.1) extends Executor {

  protected val scheduler = new Scheduler
  protected var board: Board = null
  
  def publish[T](topic: String, typeName: String, message: T) = {
    //TODO communication depending on the type
    //TODO gpio and also serialization over i2c, spi, uart, ...
    typeName match {
      case std_msgs.Bool._TYPE =>
        val msg = message.asInstanceOf[std_msgs.Bool]
        ???
      case std_msgs.Byte._TYPE =>
        val msg = message.asInstanceOf[std_msgs.Byte]
        ???
      case std_msgs.Int16._TYPE =>
        val msg = message.asInstanceOf[std_msgs.Int16]
        ???
      case std_msgs.Int32._TYPE =>
        val msg = message.asInstanceOf[std_msgs.Int32]
        ???
      case other =>
        sys.error (other + " is not yet supported with react's bulldog executor")
    }
  }
  
  protected val subscribers = scala.collection.mutable.Map[String, Any]()
  def getSubscriber[T](topic: String, typeName: String): org.ros.node.topic.Subscriber[T] = {
    //for Bool we could use bulldog.devices.switches.Button{Listener}
    //for the others (analog input) ? AnalogInput with a frequency and a ThresholdListener
    ???
  }
  
  private val messageFactory = NodeConfiguration.newPrivate().getTopicMessageFactory()
  /* conversion between REACT and ROS messages */
  def convertMessage[N](msg: Message): N = {
    Message.toMessage(messageFactory, msg).asInstanceOf[N] //TODO something nicer to avoid the casting
  }

  def schedule(t: react.runtime.ScheduledTask) = scheduler.schedule(t)

  def removeCanceledTask: Unit = scheduler.removeCanceled
  
  protected def initialize {
    assert(board == null, "already initialized")
    board = Platform.createBoard()
    ???
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
  
  override def finalize {
    ???
  }

}

class AnalogBulldogSubscriber( board: Board, node: MessageFactory, periodMS: Int, threshold: Double,
                               topic: String, tpe: String) extends BulldogSubscriber[std_msgs.Float64](topic, tpe)
                                                           with ThresholdListener {
  assert(tpe == std_msgs.Float64._TYPE, "Analog input should use Float64 types")
  //TODO check that the pin is indeed an analog pin

  protected val pin = board.getPin(topic).as(classOf[AnalogInput])
  pin.startMonitor(periodMS, this)

  protected var lastValue = -1.0
  
  def thresholdReached {
    val msg = node.newFromType[std_msgs.Float64](tpe)
    msg.setData(lastValue)
    listeners.foreach(_.onNewMessage(msg))
  }

  def isThresholdReached(value: Double) = {
    if ((value - lastValue).abs >= threshold) {
      lastValue = value
      true  //thresholdReached is going to be called next
    } else {
      false
    }
  }
  
  def shutdown {
    pin.stopMonitor()
  }
  def shutdown(timeout: Long, unit: TimeUnit) {
    shutdown
  }

}

class DigitalBulldogSubscriber( board: Board, node: MessageFactory, debounceMS: Int,
                                topic: String, tpe: String) extends BulldogSubscriber[std_msgs.Bool](topic, tpe)
                                                            with ButtonListener {
  assert(tpe == std_msgs.Bool._TYPE, "DigitalInput input should use Bool type")
  //TODO check that the pin is indeed an digital pin

  protected val pin = board.getPin(topic).as(classOf[DigitalInput])
  protected val button = new Button(pin, debounceMS, Signal.Low)

  button.addListener(this)

  def buttonPressed() {
    val msg = node.newFromType[std_msgs.Bool](tpe)
    msg.setData(false)
    listeners.foreach(_.onNewMessage(msg))
  }

  def buttonReleased() {
    val msg = node.newFromType[std_msgs.Bool](tpe)
    msg.setData(true)
    listeners.foreach(_.onNewMessage(msg))
  }

  def shutdown {
    pin.clearInterruptListeners()
  }
  def shutdown(timeout: Long, unit: TimeUnit) {
    shutdown
  }

}


abstract class BulldogSubscriber[T]( topic: String, tpe: String) extends org.ros.node.topic.Subscriber[T] {

  import org.ros.node.topic._
  import org.ros.message.MessageListener

  protected val lock = new ReentrantLock() //TODO a read write lock

  protected var listeners: List[MessageListener[T]] = Nil
  def addMessageListener(listener: MessageListener[T], limit: Int) = addMessageListener(listener)
  def addMessageListener(listener: MessageListener[T]) {
    lock.lock //TODO write lock
    try {
      listeners = listener :: listeners
    } finally {
      lock.unlock
    }
  }

  def getLatchMode = false
  def addSubscriberListener(listener: SubscriberListener[T]) {
    sys.error("addSubscriberListener not supported for Bulldog")
  }

  def getTopicMessageType = tpe
  def getTopicName = org.ros.namespace.GraphName.of(topic)

  /** delivers a message */
  def message(msg: T) = {
    //TODO read lock
    listeners.foreach(_.onNewMessage(msg))
  }

}
