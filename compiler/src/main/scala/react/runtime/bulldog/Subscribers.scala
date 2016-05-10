package react.runtime.bulldog

import react._
import react.message._
import org.ros.message.{MessageFactory,MessageListener}
import org.ros.node.topic._
import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.ReentrantLock
import io.silverspoon.bulldog.core.platform.{Platform,Board}
import io.silverspoon.bulldog.core.gpio.DigitalInput
import io.silverspoon.bulldog.core.pwm.AnalogInput
import io.silverspoon.bulldog.core.event.ThresholdListener
import io.silverspoon.bulldog.core.Signal
import io.silverspoon.bulldog.core.util.BulldogUtil
import io.silverspoon.bulldog.devices.switches.{Button,ButtonListener}

abstract class Subscriber[T]( topic: String, tpe: String) extends org.ros.node.topic.Subscriber[T] {


  protected val lock = new ReentrantLock() //TODO a read write lock

  protected var listeners: List[MessageListener[T]] = Nil
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
    sys.error("addSubscriberListener not supported for Bulldog")
  }

  def getTopicMessageType = tpe
  def getTopicName = org.ros.namespace.GraphName.of(topic)

  /** delivers a message */
  def message(msg: T) = {
    lock.lock
    try {
      listeners.foreach(_.onNewMessage(msg))
    } finally {
      lock.unlock
    }
  }
  
  def shutdown(timeout: Long, unit: TimeUnit) { shutdown() }

}

class AnalogSubscriber( board: Board, node: MessageFactory, topic: String,
                        periodMS: Int, threshold: Double)
  extends Subscriber[std_msgs.Float64](topic, std_msgs.Float64._TYPE)
  with ThresholdListener {

  //TODO check that the pin is indeed an analog pin

  protected val pin = board.getPin(topic).as(classOf[AnalogInput])
  pin.startMonitor(periodMS, this)

  protected var lastValue = -1.0
  
  def thresholdReached {
    val msg = node.newFromType[std_msgs.Float64](std_msgs.Float64._TYPE)
    msg.setData(lastValue)
    message(msg)
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

}

class DigitalSubscriber( board: Board, node: MessageFactory, topic: String, debounceMS: Int)
    extends Subscriber[std_msgs.Bool](topic, std_msgs.Bool._TYPE)
    with ButtonListener {

  //TODO check that the pin is indeed an digital pin

  protected val pin = board.getPin(topic).as(classOf[DigitalInput])
  protected val button = new Button(pin, debounceMS, Signal.Low)

  button.addListener(this)

  @inline private def evt(b: Boolean) {
    val msg = node.newFromType[std_msgs.Bool](std_msgs.Bool._TYPE)
    msg.setData(b)
    message(msg)
  }

  def buttonPressed() { evt(false) }

  def buttonReleased() { evt(true) }

  def shutdown {
    pin.clearInterruptListeners()
  }

}


