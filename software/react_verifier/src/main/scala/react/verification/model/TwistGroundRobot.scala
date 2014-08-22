package react.verification.model

import react.message._
import react.verification.environment._
import math._


/** Model for an ideal (execute command perfectly) robot moving on the ground */
class TwistGroundRobot( bBox: Box2D,
                        topic: String,
                        cmdTime: String) {

    //TODO can carry sensor

    //TODO need to register on the given topic and wait for Twist messages

    var x = 0.0
    var y = 0.0
    //var z = 0.0
    var orientation = 0.0

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

    protected def updateChildrenPose {
      sys.error("TODO")
    }

    val nullCommand = Twist(Vector3(0,0,0), Vector3(0,0,0))
    var commandTimeLeft = 0
    var currentCommand = nullCommand

    def speed = currentCommand.linear.x

    def elapse(t: Int) {
      val dt = min(t, commandTimeLeft)
      val vx = currentCommand.linear.x
      val vo = currentCommand.angular.z

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
        currentCommand = nullCommand
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


}
