package react.verification.model

import react._
import react.message._
import react.verification.environment._
import react.verification.ghost._
import react.verification._
import math._


/** Model for an ideal (execute command perfectly) robot moving on the ground */
class TwistGroundRobot( bBox: Box2D,
                        topic: String,
                        cmdTime: String,
                        sensors: List[(Sensor, Pose2D)] /* sensor and offset w.r.t the robot frame */
                      ) extends Executed {


    var x = 0.0
    var y = 0.0
    //var z = 0.0
    var orientation = 0.0

    /* what to execute */
    var commandTimeLeft = 0
    var vx = 0.0
    var vo = 0.0
    
    @transient
    var robotId = ""


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

    def elapse(t: Int) {
      //TODO forward to the children ...
      val dt = min(t, commandTimeLeft)

      if (vo == 0.0) {
        x += dt * vx * cos(orientation)
        y += dt * vx * sin(orientation)
      } else {
        val r = vx / vo
        val dx = r * cos(vo*dt)
        val dy = r * sin(vo*dt)
        x += dx * cos(orientation) - dy * sin(orientation)
        y += dx * sin(orientation) + dy * cos(orientation)
      }
      orientation += vo * dt

      updateChildrenPose

      commandTimeLeft -= dt
      if (commandTimeLeft <= 0) {
        vx = 0.0
        vo = 0.0
      }
    }

    val boxOffsetX = bBox.x - x
    val boxOffsetY = bBox.y - y
    val boxOffsetO = bBox.orientation - orientation

    def boundingBox = {
      //return the box that corresponds to the robot current position
      new Box2D(x + boxOffsetX * cos(orientation + boxOffsetO),
                y + boxOffsetY * sin(orientation + boxOffsetO),
                orientation + boxOffsetO,
                bBox.width,
                bBox.depth)
    }

    override def register(exec: Executor) {
      super.register(exec)
      sensors.foreach(_._1.register(exec))
    }

    override def deregister(exec: Executor) {
      super.deregister(exec)
      sensors.foreach(_._1.deregister(exec))
    }

}
