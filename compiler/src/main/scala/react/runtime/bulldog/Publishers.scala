package react.runtime.bulldog

import react._
import react.message._
import org.ros.node.topic._
import org.ros.message.MessageFactory
import java.util.concurrent.TimeUnit
import java.util.concurrent.locks.ReentrantLock
import io.silverspoon.bulldog.core.platform.{Platform,Board}
import io.silverspoon.bulldog.core.gpio.{DigitalOutput,Pwm}
import io.silverspoon.bulldog.core.Signal
import io.silverspoon.bulldog.core.util.BulldogUtil
import io.silverspoon.bulldog.devices.servo.{Servo,TowerProMicroSG90}


abstract class Publisher[T]( topic: String, tpe: String) extends org.ros.node.topic.Publisher[T] {
  
  //def newMessage: T
  //def publish(message: T)
  //def shutdown { }
  //def shutdown(timeout: Long, unit: TimeUnit) { }

  //TODO should we have a lock for the publish ?

  def addListener(listener: PublisherListener[T]) {
    sys.error("addListener not supported for Bulldog")
  }

  def getLatchMode = false
  def setLatchMode(enabled: Boolean) {
    sys.error("setLatchMode not yet supported")
  }

  //the hardware is the subscriber
  def getNumberOfSubscribers = 1
  def hasSubscribers = true

  def getTopicMessageType = tpe
  def getTopicName = org.ros.namespace.GraphName.of(topic)

  def shutdown(timeout: Long, unit: TimeUnit) { shutdown() }

}

class DigitalPublisher( board: Board, node: MessageFactory, topic: String) extends Publisher[std_msgs.Bool](topic, std_msgs.Bool._TYPE) {

  //TODO check that the topic/pin is indeed an gpio pin

  protected val pin = board.getPin(topic).as(classOf[DigitalOutput])
  
  def newMessage: std_msgs.Bool = node.newFromType[std_msgs.Bool](std_msgs.Bool._TYPE)

  def publish(message: std_msgs.Bool) {
    val d = message.getData
    if (d) pin.high()
    else   pin.low()
  }

  def shutdown { }
}

class PwmPublisher( board: Board, node: MessageFactory, topic: String) extends Publisher[std_msgs.Float64](topic, std_msgs.Float64._TYPE) {

  //TODO check that the topic/pin is indeed an PWM pin

  protected val pwm = board.getPin(topic).as(classOf[Pwm])
  //pwm.enable XXX this gives an error

  def newMessage: std_msgs.Float64 = node.newFromType[std_msgs.Float64](std_msgs.Float64._TYPE)

  def publish(message: std_msgs.Float64) {
    val d = message.getData
    pwm.setDuty(d)
  }

  def shutdown { pwm.disable }

}

class ServoPublisher( board: Board, node: MessageFactory, topic: String) extends PwmPublisher(board, node, topic) {

  //protected val servo = new Servo(pwm)
  protected val servo = new TowerProMicroSG90(pwm)

  override def publish(message: std_msgs.Float64) {
    val d = message.getData
    servo.moveAsyncTo(d)
  }

}

class SmoothServoPublisher( board: Board, node: MessageFactory, topic: String) extends ServoPublisher(board, node, topic) {

  override def publish(message: std_msgs.Float64) {
    val d = message.getData
    servo.moveSmoothAsyncTo(d)
  }

}

class LinearServoPublisher( board: Board, node: MessageFactory, topic: String, duration: Int) extends ServoPublisher(board, node, topic) {

  override def publish(message: std_msgs.Float64) {
    val d = message.getData
    servo.moveAsyncTo(d, duration)
  }

}

class TimedSmoothServoPublisher( board: Board, node: MessageFactory, topic: String, duration: Int) extends ServoPublisher(board, node, topic) {

  override def publish(message: std_msgs.Float64) {
    val d = message.getData
    servo.moveSmoothAsyncTo(d, duration)
  }

}

