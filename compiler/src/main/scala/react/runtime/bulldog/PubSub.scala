package react.runtime.bulldog

import react._
import react.message._
import org.ros.node.topic._
import org.ros.message.MessageFactory
import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.ReentrantLock
import io.silverspoon.bulldog.core.platform.{Platform,Board}
import io.silverspoon.bulldog.core.Signal
import io.silverspoon.bulldog.core.util.BulldogUtil
import io.silverspoon.bulldog.core.io.serial.{SerialPort,SerialDataListener,SerialDataEventArgs}

//TODO
//  for spi, i2c, it is a bit more complex (those are buses that have one master and many slaves ...)

abstract class PubSub[T]( topic: String, tpe: String) extends Subscriber[T](topic, tpe)
                                                      with org.ros.node.topic.Publisher[T] {

  //def newMessage: T
  //def publish(message: T)
  //def shutdown { }

  def addListener(listener: PublisherListener[T]) {
    sys.error("addListener not supported for Bulldog")
  }

  def setLatchMode(enabled: Boolean) {
    sys.error("setLatchMode not yet supported")
  }

  //the hardware is the subscriber
  def getNumberOfSubscribers = 1
  def hasSubscribers = true

}

class UARTPubSub(board: Board, node: MessageFactory, topic: String,
                 baudRate: Int, blocking: Boolean)
    extends PubSub[std_msgs.String](topic, std_msgs.String._TYPE)
    with SerialDataListener {

  //TODO check that the topic is indeed an UART

  val serial = board.getSerialPort(topic)
  serial.setBaudRate(baudRate)
  serial.setBlocking(blocking)
  serial.open();
  
  def newMessage = node.newFromType[std_msgs.String](std_msgs.String._TYPE)

  def publish(message: std_msgs.String) {
    val s = message.getData
    lock.lock
    try {
      serial.writeString(s)
    } finally {
      lock.unlock
    }
  }
  
  def onSerialDataAvailable(args: SerialDataEventArgs) {
    val s = args.getDataAsString
    val m = newMessage
    m.setData(s)
    message(m)
  }
  
  def shutdown {
    lock.lock
    try {
      serial.close
    } finally {
      lock.unlock
    }
  }

}
