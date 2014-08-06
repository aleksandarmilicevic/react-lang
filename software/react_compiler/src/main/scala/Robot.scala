package react

import react.verification.World
import react.message._

import scala.language.experimental.macros

abstract class Robot {

  def on[T](handler: PartialFunction[T, Unit]) = {
    sys.error("TODO")
    //TODO how to dispatch event in a typesafe way ?
  }

  def every(period: Int)(body: () => Unit) = {
    sys.error("TODO")
  }

  //TODO the runtime stuff and the messages ...
  //connections
  //discovery of robot by react ?

  def shadow: Unit
  def generateMvmt(period: Int): Seq[Mvmt]


  //helper to simplify message generation
  private var seq = 0
  protected def nextHeader = {
    val s = seq
    seq += 1
    val t = Message.time(System.currentTimeMillis())
    val frame = "1"
    Header(s, t, frame)
  }
}

object Robot {

  import react.rewriting.RobotMacros
  import react.verification.World

  implicit class Explorable[M <: Robot](val robot: M) extends AnyVal {
    def serialize(world: World): Array[Byte] = macro RobotMacros.toWord[M]
    def deserilize(world: World, state: Array[Byte]): Unit = macro RobotMacros.fromWord[M]
  }

}

abstract class GroundRobot extends Robot {

  //ROS Pose2D
  var x = 0.0
  var y = 0.0
  var theta = 0.0

  private var shadow_x = 0.0
  private var shadow_y = 0.0
  private var shadow_theta = 0.0

  def shadow = {
    shadow_x = x
    shadow_y = y
    shadow_theta = theta
  }

  def generateMvmt(period: Int) = {
    val f = 0.5 //parameter f ∈ (0,1) to control how tight is the maneuver

    val t = period / 1000.0
    val dx = x - shadow_x
    val dy = y - shadow_y
    val dT = theta - shadow_theta
    val v = math.sqrt(dx*dx + dy*dy)

    val t1 = 0.75 * f * t
    val v1 = v * 2 * dT / (math.sin(dT) + 2*math.sin(dT/2.0))
    val o1 = 2 * dT / (f * t)

    val t2 = 0.25 * f * t
    val v2 = v1
    val o2 = o1

    val t3 = (1-f) * t
    val v3 = v
    val o3 = 0
    
    val m1 = Mvmt(nextHeader, v1, o1, Message.duration((t1 * 1000).toLong))
    val m2 = Mvmt(nextHeader, v2, o2, Message.duration((t2 * 1000).toLong))
    val m3 = Mvmt(nextHeader, v3, o3, Message.duration((t3 * 1000).toLong))
    Seq(m1, m2, m3)
  }

}

//abstract class FlyingRobot extends Robot {
//  ROS Pose
//  var position: Vector3D
//  var orientation: Vector3D //actually should be a quaternion
//}

