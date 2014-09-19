package react.verification.model

import react._
import react.robot._
import react.message._
import react.verification.environment._
import react.verification.ghost._
import react.verification._
import math._

//TODO uncertainty according rounding

class GroundRobot( bBox: Box2D,
                   snap: Option[(String,String)] = None
                 ) extends Executed {

  val lock = new java.util.concurrent.locks.ReentrantLock(true)

  /* sensor and offset w.r.t the robot frame */
  @ignore var sensors: List[(Sensor, Pose2D)] = Nil

  var x = 0.0
  var y = 0.0
  var orientation = 0.0
  
  var vx = 0.0
  var vo = 0.0

  override def toString = {
    "robot model: x = " + x + ", y = " + y + ", Θ = " + orientation + ", vx = " + vx + ", vΘ = " + vo
  }

  @ignore
  var robotId = ""

  def addSensor(s: Sensor, p: Pose2D) {
    sensors = (s,p) :: sensors
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

  def setPose(pose: Pose2D) = {
    x = pose.x
    y = pose.y
    orientation = pose.theta
    updateChildrenPose
  }

  protected def setChildPose(s: Sensor, offset: Pose2D) {
    val cx = x + cos(orientation) * offset.x - sin(orientation) * offset.y
    val cy = y + sin(orientation) * offset.x + cos(orientation) * offset.y
    val co = orientation + offset.theta
    s.pose = Pose2D(cx, cy, co)
  }

  protected def updateChildrenPose {
    for ( (s, p) <- sensors )
      setChildPose(s, p)
  }

  def restored {
    updateChildrenPose
  }

  //rest of the world exclude the robot own bounding box
  //this method will be called each time the world changes
  def updateWorld(restOfTheWorld: List[Box2D]) {
    sensors.foreach(_._1.update(restOfTheWorld))
  }

  protected def moveFor(t: Int) = {
    val dt = t / 1000.0
    if (vo == 0.0) {
      x += dt * vx * cos(orientation)
      y += dt * vx * sin(orientation)
    } else {
      //TODO this is wrong !!
      val r = vx / vo
      val dx = r * cos(vo*dt)
      val dy = r * sin(vo*dt)
      x += dx * cos(orientation) - dy * sin(orientation)
      y += dx * sin(orientation) + dy * cos(orientation)
    }
    orientation += vo * dt
  }

  def elapse(t: Int) {
    println(this.toString)
    moveFor(t)
    updateChildrenPose
    println("--> " + this.toString)
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

  override def register(exec: Executor) {
    super.register(exec)
    if (snap.isDefined) {
      val listener2 = new org.ros.message.MessageListener[gazebo_msgs.ModelState]{
        val name = snap.get._2
        def onNewMessage(message: gazebo_msgs.ModelState) {
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
        }
      }
      val sub = exec.getSubscriber[gazebo_msgs.ModelState](snap.get._1, gazebo_msgs.ModelState._TYPE)
      sub.addMessageListener(listener2)
    }
    sensors.foreach(_._1.register(exec))
  }

  override def deregister(exec: Executor) {
    super.deregister(exec)
    sensors.foreach(_._1.deregister(exec))
  }

}
