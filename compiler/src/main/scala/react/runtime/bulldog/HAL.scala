package react.runtime.bulldog

import react._
import react.message._
import org.ros.message.MessageFactory
import java.util.concurrent.TimeUnit
import io.silverspoon.bulldog.core.platform.{Platform,Board}
import io.silverspoon.bulldog.core.gpio.DigitalInput
import io.silverspoon.bulldog.core.pwm.AnalogInput
import io.silverspoon.bulldog.core.event.ThresholdListener
import io.silverspoon.bulldog.core.Signal
import io.silverspoon.bulldog.core.util.BulldogUtil
import io.silverspoon.bulldog.devices.switches.{Button,ButtonListener}

/** An hardware abstraction layer for project using libBulldog.
 *
 *  By default:
 *  - Boolean messages maps to GPIO
 *  - Double/Float64 maps to AnalogInput and Pwm output
 *  - String maps to UART serial port
 *  
 *  To change the default, you should override the initialize method (don't forget to call super.initialize)
 *  and populate the publishers/subscribers/pubsubs maps with the appropriate elements.
 */
trait HAL {

  protected var board: Board = null
  protected var mFactory: MessageFactory = null

  def initialize(m: MessageFactory) {
    assert(board == null, "already initialized")
    board = Platform.createBoard()
    mFactory = m
  }

  def shutdown {
    publishers.values.foreach( p => p.asInstanceOf[Publisher[_]].shutdown() )
    subscribers.values.foreach( p => p.asInstanceOf[Subscriber[_]].shutdown() )
    pubsubs.values.foreach( p => p.asInstanceOf[PubSub[_]].shutdown() )
    board.shutdown
    board = null
    mFactory = null
  }

  /** default debounce time for digital IO */
  val debounceMS: Int = 0

  /** for analog input, the default period at which the pin is polled */
  val periodMS: Int = 5

  /** for analog input, the default change threshold to fire an event */
  val threshold: Double = 0.1

  /** for UART */
  val baudRate: Int = 9600

  /** for UART */
  val blocking: Boolean = false

  protected val publishers = scala.collection.mutable.Map[String, Any]()
  protected val subscribers = scala.collection.mutable.Map[String, Any]()
  protected val pubsubs = scala.collection.mutable.Map[String, Any]()

  def getSubscriber[T](topic: String, tpe: String): org.ros.node.topic.Subscriber[T] = {
    if (subscribers contains topic) {
      subscribers(topic).asInstanceOf[Subscriber[T]]
    } else if (pubsubs contains topic) {
      pubsubs(topic).asInstanceOf[PubSub[T]]
    } else {
      tpe match {
        case std_msgs.Bool._TYPE =>
          val sub = new DigitalSubscriber(board, mFactory, topic, debounceMS)
          subscribers += topic -> sub
          sub.asInstanceOf[Subscriber[T]]
        case std_msgs.Float64._TYPE =>
          val sub = new AnalogSubscriber(board, mFactory, topic, periodMS, threshold)
          subscribers += topic -> sub
          sub.asInstanceOf[Subscriber[T]]
        case std_msgs.String._TYPE =>
          val ps = new UARTPubSub(board, mFactory, topic, baudRate, blocking)
          pubsubs += topic -> ps
          ps.asInstanceOf[PubSub[T]]
        case other =>
          sys.error (other + " is not yet supported with react's bulldog executor")
      }
    }
  }

  def getPublisher[T](topic: String, tpe: String): org.ros.node.topic.Publisher[T] = {
    if (publishers contains topic) {
      publishers(topic).asInstanceOf[Publisher[T]]
    } else if (pubsubs contains topic) {
      pubsubs(topic).asInstanceOf[PubSub[T]]
    } else {
      tpe match {
        case std_msgs.Bool._TYPE =>
          val pub = new DigitalPublisher(board, mFactory, topic)
          publishers += topic -> pub
          pub.asInstanceOf[Publisher[T]]
        case std_msgs.Float64._TYPE =>
          val pub = new PwmPublisher(board, mFactory, topic)
          publishers += topic -> pub
          pub.asInstanceOf[Publisher[T]]
        case std_msgs.String._TYPE =>
          val ps = new UARTPubSub(board, mFactory, topic, baudRate, blocking)
          pubsubs += topic -> ps
          ps.asInstanceOf[PubSub[T]]
        case other =>
          sys.error (other + " is not yet supported with react's bulldog executor")
      }
    }
  }

}
