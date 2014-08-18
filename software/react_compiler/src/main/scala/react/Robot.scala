package react

import react.message._

import scala.language.experimental.macros

import react.rewriting.{RobotMacros, ExplorableMacros}

//TODO the contructor should be private, robot should have a factory that ensures we create node using RosRun

abstract class Robot(val id: String) {

  def on(handler: PartialFunction[Any, Unit]) = {
    handlers = handler :: handlers
  }

  def every(period: Int)(body: Unit): Unit = macro RobotMacros.every
  
  /** subscribe to a topic using the REACT messages */
  def sensor[T <: Message](source: String)(handler: PartialFunction[T, Unit]): Unit = macro RobotMacros.registerHandler[T]

  /** subscribe to a topic directly using ROS messages */
  def subscribe[T](source: String, msgType: String)(handler: T => Unit) = {
    val registerFct = ( (exec: react.runtime.RobotExecutor) => {
      exec.subscribe[T](source, msgType, handler)
    } )
    sensors = registerFct :: sensors
  }

  /** publishing using REACT message type */
  def publish[T <: Message](topic: String, message: T): Unit = macro RobotMacros.publish[T]

  /** publishing using ROS native message type */
  def publish[T](topic: String, typeName: String, message: T) = {
    exec.publish(topic, typeName, message)
  }

  /////////////////////
  // for the runtime //
  /////////////////////
  
  protected var exec: react.runtime.RobotExecutor = null
  def setExec(n: react.runtime.RobotExecutor) {
    assert(exec == null, "setNode should be used only be the REACT runtime, thanks")
    exec = n
    for(s <- sensors) s(exec)
  }

  /** create a copy of the physical state of the robot, used later by generateMvmt (to compute pre/post difference) */
  //def shadow: Unit = { }

  /** generate a sequence of messages for the robot to execute (match the physical state to the model state) */
  //def generateMvmt(period: Int): Seq[Message] = Seq()

  protected var _tasks: List[(Int, (() => Unit))] = Nil
  protected var handlers: List[PartialFunction[Any, Unit]] = Nil
  protected var sensors: List[(react.runtime.RobotExecutor => Unit)] = Nil

  def tasks = _tasks

  //TODO get rid of that everything should be ros message only
  def send(any: Any) {
    val defined = handlers.filter(_.isDefinedAt(any))
    lock.lock
    try {
      defined.foreach(_.apply(any))
    } finally {
      lock.unlock
    }
  }

  //helper to simplify message generation
  private var seq = 0
  protected def nextHeader = {
    val s = seq
    seq += 1
    val t = Message.time(System.currentTimeMillis())
    val frame = "1"
    Header(s, t, frame)
  }

  //TODO move the lock to the executor ?
  val lock = new java.util.concurrent.locks.ReentrantLock

}

object Robot {

  import java.nio.ByteBuffer

  implicit class Explorable[M <: Robot](val robot: M) extends AnyVal {
    def length(world: World): Int = macro ExplorableMacros.wordLength[M] //in byte
    def serialize(world: World, out: ByteBuffer): Unit = macro ExplorableMacros.toWord[M]
    def deserilize(world: World, in: ByteBuffer): Unit = macro ExplorableMacros.fromWord[M]
  }

}

abstract class GroundRobot(_id: String) extends Robot(_id) {

  //ROS Pose2D
  var x = 0.0
  var y = 0.0
  var orientation = 0.0

//private var shadow_x = 0.0
//private var shadow_y = 0.0
//private var shadow_orientation = 0.0

//override def shadow = {
//  shadow_x = x
//  shadow_y = y
//  shadow_orientation = orientation
//}

////TODO allows for delayed actions (Twist do not have a duration)
//override def generateMvmt(period: Int) = {
//  val f = 0.5 //parameter f ∈ (0,1) to control how tight is the maneuver

//  val t = period / 1000.0
//  val dx = x - shadow_x
//  val dy = y - shadow_y
//  val dT = orientation - shadow_orientation
//  val v = math.sqrt(dx*dx + dy*dy)

//  val t1 = 0.75 * f * t
//  val v1 = v * 2 * dT / (math.sin(dT) + 2*math.sin(dT/2.0))
//  val o1 = 2 * dT / (f * t)

//  val t2 = 0.25 * f * t
//  val v2 = v1
//  val o2 = o1

//  val t3 = (1-f) * t
//  val v3 = v
//  val o3 = 0
//  
//  val m1 = Mvmt(nextHeader, v1, o1, Message.duration((t1 * 1000).toLong))
//  val m2 = Mvmt(nextHeader, v2, o2, Message.duration((t2 * 1000).toLong))
//  val m3 = Mvmt(nextHeader, v3, o3, Message.duration((t3 * 1000).toLong))
//  Seq(m1, m2, m3)
//}

}

//abstract class FlyingRobot extends Robot {
//  ROS Pose
//  var position: Vector3D
//  var orientation: Vector3D //actually should be a quaternion
//}

