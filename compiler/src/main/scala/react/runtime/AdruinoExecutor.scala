package react.runtime

import react._
import react.message._
import org.ros.namespace.GraphName
import org.ros.node.NodeConfiguration
import org.ros.concurrent.CancellableLoop
import java.util.concurrent.locks.ReentrantLock
import java.util.concurrent.TimeUnit
import jssc._ //java simple serial connector

class ArduinoExecutor(val robot: Robot, binary: Boolean = false, poll: Option[Int] = None) extends Executor {

  val scheduler = new Scheduler

  lazy val port: String = robot.id
  //TODO make that configurable
  var baudRate: Int = SerialPort.BAUDRATE_9600
  //var baudRate: Int = SerialPort.BAUDRATE_38400
  var dataBits: Int = SerialPort.DATABITS_8
  var stopBits: Int = SerialPort.STOPBITS_1
  var parity: Int = SerialPort.PARITY_NONE

  //port
  private var opened = false
  private lazy val serialPort = new SerialPort(port)

  protected def initialize {
    if (!opened) {
      opened = true
      serialPort.openPort()
      serialPort.setParams(baudRate, dataBits, stopBits, parity) 
      val dispatcher =
        if (binary) new BinarySerialListener(serialPort, this)
        else new StringSerialListener(serialPort, this)
      serialPort.addEventListener(dispatcher)
    }
  }

  protected def close {
    if (opened) {
      serialPort.closePort();
      opened = false
    }
  }

  protected def publishBinary[T](topic: String, typeName: String, message: T) = {
    assert(Primitive.is(typeName), "can only send primitive types to Arduino")
    val payload = Array.ofDim[Byte](3)
    //payload(0) = Arduino.portToInt(topic)
    payload(0) = topic.toByte
    typeName match {
      case std_msgs.Bool._TYPE =>
        val msg = message.asInstanceOf[std_msgs.Bool]
        if (msg.getData) payload(1) = 1
      case std_msgs.Byte._TYPE =>
        val msg = message.asInstanceOf[std_msgs.Byte]
        payload(1) = msg.getData
      case std_msgs.Int16._TYPE =>
        val msg = message.asInstanceOf[std_msgs.Int16]
        val d = msg.getData
        payload(1) = d.toByte
        payload(2) = (d >>> 8).toByte
      case other => sys.error (other + " is not yet supported in the arduino communication")
    }
    //println("publishing: " + payload.mkString(", "))
    serialPort.writeBytes(payload)
  }

  //the format used by Josef, Ankur
  def publishString[T](topic: String, typeName: String, message: T) = {
    val value = typeName match {
      case std_msgs.Bool._TYPE =>
        val msg = message.asInstanceOf[std_msgs.Bool]
        if (msg.getData) "1" else "0"
      case std_msgs.Byte._TYPE =>
        val msg = message.asInstanceOf[std_msgs.Byte]
        msg.getData.toString
      case std_msgs.Int16._TYPE =>
        val msg = message.asInstanceOf[std_msgs.Int16]
        msg.getData.toString
      case other => sys.error (other + " is not yet supported in the arduino communication")
    }
    //val msg = "DATA$"+topic+"$"+value+"\u0000"
    //val msg = "DATA$"+value+"$$"+topic+"\u0000"
    val msg = "DATA$"+value+"$"+topic+"$-1\u0000"
    //val msg = "DATA$"+value+"$0$"+topic+"\u0000"
    val bytes = msg.getBytes("ASCII")
    println("publishing: " + msg)
    serialPort.writeBytes(bytes)
  }

  def publish[T](topic: String, typeName: String, message: T) = {
    if (binary) publishBinary(topic, typeName, message)
    else publishString(topic, typeName, message)
  }

  def dispatch(port: Byte, value: Short) {
    //val topic = Arduino.intToPort(port)
    //println("dispatching: " + port + ", " + value)
    val topic = port.toString
    if (subscribers contains topic) {
      val sub = subscribers(topic).asInstanceOf[ArduinoSubscriber[_]]
      val tpe = sub.getTopicMessageType
      if (tpe == std_msgs.Bool._TYPE) {
          val typedSub = sub.asInstanceOf[ArduinoSubscriber[std_msgs.Bool]]
          val msg = convertMessage[std_msgs.Bool](Primitive.Bool(value != 0))
          typedSub.message(msg)
      } else if (tpe == std_msgs.Byte._TYPE) {
          val typedSub = sub.asInstanceOf[ArduinoSubscriber[std_msgs.Byte]]
          val msg = convertMessage[std_msgs.Byte](Primitive.Byte((value & 0xFF).toByte))
          typedSub.message(msg)
      } else if (tpe == std_msgs.Int16._TYPE) {
          val typedSub = sub.asInstanceOf[ArduinoSubscriber[std_msgs.Int16]]
          val msg = convertMessage[std_msgs.Int16](Primitive.Int16(value))
          typedSub.message(msg)
      } else {
        sys.error(tpe + " is not yet supported in the arduino communication")
      }
    }
  }

  val subscribers = scala.collection.mutable.Map[String, Any]()
  def getSubscriber[T](topic: String, typeName: String): org.ros.node.topic.Subscriber[T] = {
    if (subscribers contains topic) {
      subscribers(topic).asInstanceOf[ArduinoSubscriber[T]]
    } else {
      val p = new ArduinoSubscriber[T](topic, typeName)
      subscribers += (topic -> p)
      poll match {
        case Some(period) =>
          if (binary) {
            Console.err.println("poll only for string format")
          } else {
            println("polling at " + period)
            val msg = ("GET$"+topic+"\u0000").getBytes("ASCII")
            //val msg = ("GET$0$"+topic+"\u0000").getBytes("ASCII")
            def pollFct() {
              println("polling: " + msg.map(_.toChar).mkString)
              serialPort.writeBytes(msg)
            }
            val task = new ScheduledTask("polling " + topic, period, pollFct)
            schedule(task)
          }
        case None =>
      }
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
        case None => Thread.sleep(1)
      }
    }
  }
  
  override def finalize {
    close
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
  
abstract class ArduinoSerialListener(serialPort: SerialPort, exec: ArduinoExecutor) extends SerialPortEventListener {
    
  protected def push(byte: Byte): Unit

  def serialEvent(event: SerialPortEvent) {
    if(event.isRXCHAR()){//If data is available
      val size = event.getEventValue()
      val buffer = serialPort.readBytes(size);
      for (b <- buffer) push(b)
      //for (b <- buffer) Console.print(b.toChar)
    } else {
      //TODO some other event
    }
  }
}

//current format:
//   messages are 3 bytes
//     [1] is the port
//     [2-3] is the value as a Short in little endian format
class BinarySerialListener(serialPort: SerialPort, exec: ArduinoExecutor) extends ArduinoSerialListener(serialPort, exec) {

  var port: Byte = 0
  var acc: Short = 0
  var i = -1

  protected def push(byte: Byte) {
    if (i == -1) {
      port = byte
    } else {
      //Console.println("pushing: " + byte + " @ " + i)
      acc = ((acc.toInt & 0xFFFF) >>> 8).toShort
      acc = (acc | ((byte.toInt << 8) & 0xFF00)).toShort
    }
    i += 1
    if (i >= 2) {
      exec.dispatch(port, acc)
      port = 0
      acc = 0
      i = -1
    }
  }

}

class StringSerialListener(serialPort: SerialPort, exec: ArduinoExecutor) extends ArduinoSerialListener(serialPort, exec) {

  val acc = new StringBuilder()

  protected def push(byte: Byte) {
    //println("receiving: " + byte + ", " + byte.toChar)
    if (byte == 0) { // '\0'
      val msg = acc.toString
      println("received: " + msg)
      acc.clear
      val parts = msg.split("\\$")
      if (parts.size == 3 || parts.size == 4) {
        if (parts(0) == "DATA") {
          try {
            val value = parts(1).toShort
            val source = parts(2).toByte
            //println("dispatching: " + value + " to port " + source)
            exec.dispatch(source, value)
            //ignore the outputfor the moment
          } catch {
            case e: Exception =>
              Console.err.println("error while parsing message: " + e + ", " + msg)
          }
        } else if (parts(0) == "GET") {
          Console.err.println("GET messages not currently supported: " + msg)
        }
      } else {
        Console.err.println("invalid message: " + msg)
      }
    } else {
      acc.append(byte.toChar)
    }
  }

}


object Arduino {
  final val A0  = "A0"
  final val A1  = "A1"
  final val A2  = "A2"
  final val A3  = "A3"
  final val A4  = "A4"
  final val A5  = "A5"
  final val D0  = "D0"
  final val D1  = "D1"
  final val D2  = "D2"
  final val D3  = "D3"
  final val D4  = "D4"
  final val D5  = "D5"
  final val D6  = "D6"
  final val D7  = "D7"
  final val D8  = "D8"
  final val D9  = "D9"
  final val D10 = "D10"
  final val D11 = "D11"
  final val D12 = "D12"
  final val D13 = "D13"

  def isAnalog(port: String) = port.startsWith("A")
  def isDigital(port: String) = port.startsWith("D")

  //this is for the UNO
  def portToInt(port: String): Byte = port match {
    case `A0` =>    14
    case `A1` =>    15
    case `A2` =>    16
    case `A3` =>    17
    case `A4` =>    18
    case `A5` =>    19
    case `D0` =>    0
    case `D1` =>    1
    case `D2` =>    2
    case `D3` =>    3
    case `D4` =>    4
    case `D5` =>    5
    case `D6` =>    6
    case `D7` =>    7
    case `D8` =>    8
    case `D9` =>    9
    case `D10` =>   10
    case `D11` =>   11
    case `D12` =>   12
    case `D13` =>   13
    case other =>   sys.error(other + "is not recognized as an UNO port")
  }

  //this is for the UNO
  def intToPort(port: Int): String = port match {
    case 14 =>  A0   
    case 15 =>  A1   
    case 16 =>  A2   
    case 17 =>  A3   
    case 18 =>  A4   
    case 19 =>  A5   
    case 0  =>  D0   
    case 1  =>  D1   
    case 2  =>  D2   
    case 3  =>  D3   
    case 4  =>  D4   
    case 5  =>  D5   
    case 6  =>  D6   
    case 7  =>  D7   
    case 8  =>  D8   
    case 9  =>  D9   
    case 10  => D10  
    case 11  => D11  
    case 12  => D12  
    case 13  => D13  
    case _ =>   sys.error("")
  }

}

