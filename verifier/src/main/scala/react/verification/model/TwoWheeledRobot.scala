package react.verification.model

import react._
import react.robot._
import react.message._
import react.runtime.MessageListenerRW
import react.verification.environment._
import react.verification.ghost._
import react.verification._
import react.verification.modelchecker.BranchingPoint
import math._
import react.utils._

/** A robot with two wheels where the axis of rotation is Y and the frame origin is exactly in the middle of the two wheels.
 *  This is a model for a robot of type (2,0) in the nomenclature of [Campion, Bastin, D'Andrea-Novel, 96].
 */
class TwoWheeledRobot( bBox: Box2D,
                       val port: String,
                       val leftWheelPin: String,
                       val rightWheelPin: String,
                       val wheelRadius: Double,
                       val wheelSpacing: Double
                     ) extends GroundRobot(bBox, None)
{

  var vl: Short = 0
  var vr: Short = 0

  val motorCoeff = 1.0
  //maps v ∈ [-100,100] to [- motorCoeff * π, motorCoeff * π] and then mutliply by ms/1000
  protected def angle(v: Short, ms: Int) = {
    val angularSpeed = v / 100.0 * motorCoeff * Pi  
    angularSpeed * ms / 1000
  }

  override protected def moveFor(t: Int) = {
    //println("moving at (" + vl + ", " + vr + ")")
    val dl = wheelRadius * angle(vl, t)
    val dr = wheelRadius * angle(vr, t)
    //println("dl/r = (" + dl + ", " + dr + ")")
    var dx = 0.0
    var dy = 0.0
    var dO = 0.0
    if (dl == dr) {
      dx = dl //moving straight
    } else {
      val centerOfRotation = (dr+dl) * wheelSpacing / 2 / (dr-dl)
      val angle = (dr - dl) / wheelSpacing
      //println("COR/angle = (" + centerOfRotation + ", " + angle + ")")
      dx = centerOfRotation * sin(angle)
      dy = centerOfRotation * (1.0 - cos(angle))
      dO = angle
      //println("dx/y/O = (" + dx + ", " + dy + ", " + dO + ")")
    } 
    //println("x/y/O = (" + x + ", " + y + ", " + orientation + ")")
    x += dx * cos(orientation) - dy * sin(orientation)
    y += dx * sin(orientation) + dy * cos(orientation)
    orientation += dO
    //println("x/y/O' = (" + x + ", " + y + ", " + orientation + ")")
  }

  val leftListener = new MessageListenerRW[std_msgs.Int16]{
    def robotID = port
    override def read = Some(Set())
    override def written = Some(Set("vl"))
    def onNewMessage(message: std_msgs.Int16) {
      //println(port + "/" + leftWheelPin + " got a message")
      lock.lock
      try {
        vl = message.getData
      } finally lock.unlock
      exec.messageDelivered
    }
  }

  val rightListener = new MessageListenerRW[std_msgs.Int16]{ 
    def robotID = port
    override def read = Some(Set())
    override def written = Some(Set("vr"))
    def onNewMessage(message: std_msgs.Int16) {
      //println(port + "/" + rightWheelPin + " got a message")
      lock.lock
      try {
        vr = message.getData
      } finally lock.unlock
      exec.messageDelivered
    }
  }
  
  override def register(exec: Executor) {
    super.register(exec)
    val subl = exec.getSubscriber[std_msgs.Int16](port + "/" + leftWheelPin, std_msgs.Int16._TYPE)
    subl.addMessageListener(leftListener)
    val subr = exec.getSubscriber[std_msgs.Int16](port + "/" + rightWheelPin, std_msgs.Int16._TYPE)
    subr.addMessageListener(rightListener)
  }

}
