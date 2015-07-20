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
import dzufferey.utils.LogLevel._
import dzufferey.utils.Logger

//TODO uncertainty according rounding

class GroundRobot( bBox: Box2D,
                   snap: Option[(String,String)] = None
                 ) extends FrameTransformer[Sensor] with Motion2D with Positioned with Executed {

  val lock = new java.util.concurrent.locks.ReentrantLock(true)

  var vx = 0.0
  var vo = 0.0

  override def toString = {
    "robot model: x = " + x + ", y = " + y + ", Θ = " + orientation + ", vx = " + vx + ", vΘ = " + vo
  }
  
  def writeAsSVG(writer: java.io.BufferedWriter, color: String = "black") {
    boundingBox.writeAsSVG(writer, color)
    writer.newLine
    val x2 = x + 0.5 * bBox.width * math.cos(orientation)
    val y2 = y + 0.5 * bBox.width * math.sin(orientation)
    writer.write("<line x1=\""+x+"\" y1=\""+y+"\" x2=\""+x2+"\" y2=\""+y2+"\" stroke-width=\""+math.min(0.1,bBox.depth/10)+"\" stroke=\""+color+"\"/>")
    writer.newLine
  }

  @ignore
  var robotId = ""

  def sensors = getLeafs

  def addSensor(s: Sensor, p: Pose2D) {
    addLeaf(s, p)
  }

  def setPosition(x: Double, y: Double) = {
    this.x = x
    this.y = y
    updateChildrenPose
  }

  def setOrientation(o: Double) = {
    orientation = o
    updateChildrenPose
  }

  def pose: Pose2D = {
    Pose2D(x, y, orientation)
  }

  def pose_=(p: Pose2D) {
    setPose(p)
  }

  def setPose(pose: Pose2D) = {
    x = pose.x
    y = pose.y
    orientation = pose.theta
    updateChildrenPose
  }

  protected def updateChildrenPose { updatePose(pose) }

  def restored {
    updateChildrenPose
  }

  //rest of the world exclude the robot own bounding box
  //this method will be called each time the world changes
  def updateWorld(restOfTheWorld: List[Box2D]) {
    sensors.foreach(_.update(restOfTheWorld))
  }

  val boxOffsetX = bBox.x //- x
  val boxOffsetY = bBox.y //- y
  val boxOffsetO = bBox.orientation //- orientation

  def boundingBox = {
    //return the box that corresponds to the robot current position
    new Box2D(x + boxOffsetX * cos(orientation) - boxOffsetY * sin(orientation),
              y + boxOffsetY * sin(orientation) + boxOffsetY * cos(orientation),
              orientation + boxOffsetO,
              bBox.width,
              bBox.depth)
  }

  protected def moveFor(t: Int) = {
    val dt = t / 1000.0
    val da = (vo * dt).abs
    if (vo == 0.0) {
      x += dt * vx * cos(orientation)
      y += dt * vx * sin(orientation)
    } else {
      val r = (vx / vo).abs
      val dx = r * sin(da) * vx.signum
      val dy = r * (1 - cos(da)) * vx.signum * vo.signum
      x += dx * cos(orientation) - dy * sin(orientation)
      y += dx * sin(orientation) + dy * cos(orientation)
    //println("Θ  = " + orientation)
    //println("dΘ = " + da)
    //println("dx = " + dx)
    //println("dy = " + dy)
    //println("cos(Θ) = " + cos(orientation))
    //println("sin(Θ) = " + sin(orientation))
    }
    orientation += da * vo.signum
  }

  def elapse(t: Int) {
    assert(t >= 0)
  //println(this.toString)
    moveFor(t)
    updateChildrenPose
  //println("--"+t+"--> " + this.toString)
  }

  /** that way we can deal with the imprecision in the physical model */
  def elapseBP(t: Int): BranchingPoint = {
    new BranchingPoint {
      def alternatives = 1
  
      def act(alt: Int): List[String] = {
        assert(alt == 0)
        elapse(t)
        List("elapse("+t+")")
      }
    }
  }

  override def register(exec: Executor) {
    super.register(exec)
    if (snap.isDefined) {
      val listener2 = new MessageListenerRW[gazebo_msgs.ModelState]{
        def robotID = GroundRobot.this.toString //TODO better
        override def read = Some(Set())
        override def written = Some(Set("x", "y", "orientation", "vx" , "vo"))
        val name = snap.get._2
        def onNewMessage(message: gazebo_msgs.ModelState) {
          Logger("GroundRobot", Debug, name + " -> " + message.getModelName)
          lock.lock
          try {
            if (message.getModelName == name) {
              x = message.getPose.getPosition.getX
              y = message.getPose.getPosition.getY
              orientation = Angle.thetaFromQuaternion(Message.from(message.getPose.getOrientation))
              vx = message.getTwist.getLinear.getX
              vo = message.getTwist.getAngular.getX
            }
          } finally lock.unlock
          exec.messageDelivered
        }
      }
      val sub = exec.getSubscriber[gazebo_msgs.ModelState](snap.get._1, gazebo_msgs.ModelState._TYPE)
      sub.addMessageListener(listener2)
    }
    sensors.foreach(_.register(exec))
  }

  override def deregister(exec: Executor) {
    super.deregister(exec)
    sensors.foreach(_.deregister(exec))
  }

}
