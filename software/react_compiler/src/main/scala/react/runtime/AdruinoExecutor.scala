package react.runtime

import react._
import react.message._
import org.ros.namespace.GraphName
import org.ros.node.NodeConfiguration
import org.ros.concurrent.CancellableLoop
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.TimeUnit
import jssc._ //java simple serial connector

//TODO
// a custom http://docs.rosjava.googlecode.com/hg/rosjava_core/html/javadoc/org/ros/node/topic/Subscriber.html
// a listener that dispatche messages to the right Subscriber

abstract class ArduinoExecutor extends Executor {

  val robot: Robot

  val scheduler = new Scheduler

  val port: String = robot.id
  var baudRate: Int = SerialPort.BAUDRATE_9600
  var dataBits: Int = SerialPort.DATABITS_8
  var stopBits: Int = SerialPort.STOPBITS_1
  var parity: Int = SerialPort.PARITY_NONE

  //port
  private var opened = false
  private val serialPort = new SerialPort(port)

  //TODO map topic -> subscribers ... (concurrent)

  val dispatcher = new SerialPortEventListener {
    def serialEvent(event: SerialPortEvent) {
      if(event.isRXCHAR()){//If data is available
        val size = event.getEventValue()
        val buffer = serialPort.readBytes(size);
        ???
      } else {
        //TODO some other event
      }
    }
  }

  protected def initialize {
    if (!opened) {
      opened = true
      serialPort.openPort()
      serialPort.setParams(baudRate, dataBits, stopBits, parity) 
      serialPort.addEventListener(dispatcher)
    }
  }

  protected def close {
    if (opened) {
      serialPort.closePort();
      opened = false
    }
  }


  def publish[T](topic: String, typeName: String, message: T) = {
    assert(Primitive.is(typeName), "can only send primitive types to Arduino")
    //TODO is the ROS-like publish ...
    ???
  }

  private val subscribers = scala.collection.mutable.Map[String, Any]()
  def getSubscriber[T](topic: String, typeName: String): org.ros.node.topic.Subscriber[T] = {
    if (subscribers contains topic) {
      subscribers(topic).asInstanceOf[ArduinoSubscriber[T]]
    } else {
      val p = new ArduinoSubscriber[T](topic, typeName)
      subscribers += (topic -> p)
      p
    }
  }

  private val messageFactory = NodeConfiguration.newPrivate().getTopicMessageFactory()

  /* conversion between REACT and ROS messages */
  def convertMessage[N](msg: Message): N = {
    Message.toMessage(messageFactory, msg).asInstanceOf[N] //TODO something nicer to avoid the casting
  }

  def schedule(t: react.runtime.ScheduledTask) = scheduler.schedule(t)

  def removeCanceledTask: Unit = scheduler.removeCanceled

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
        case None => ()
      }
    }
  }

}

class ArduinoSubscriber[T](topic: String, tpe: String) extends org.ros.node.topic.Subscriber[T] {
  import org.ros.node.topic._
  import org.ros.message.MessageListener
  val lock = new ReentrantLock()
  var listeners: List[MessageListener[T]] = Nil
  def addMessageListener(listener: MessageListener[T], limit: Int) = addMessageListener(listener)
  def addMessageListener(listener: MessageListener[T]) {
    lock.lock
    try {
      listeners = listener :: listeners
    } finally {
      lock.unlock
    }
  }
  def getLatchMode = false
  def addSubscriberListener(listener: SubscriberListener[T]) {
    sys.error("addSubscriberListener not supported for Arduino")
  }
  def shutdown { }
  def shutdown(timeout: Long, unit: TimeUnit) { }
  def getTopicMessageType = tpe
  def getTopicName = org.ros.namespace.GraphName.of(topic)

  /** delivers a message */
  def message(msg: T) = listeners.foreach(_.onNewMessage(msg))
}
