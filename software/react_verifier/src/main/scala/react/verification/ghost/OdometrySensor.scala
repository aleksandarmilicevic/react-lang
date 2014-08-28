package react.verification.ghost

import react._
import react.verification._
import react.message._
import react.robot._
import react.verification.model._

class OdometrySensor( bodyFrame: String, //the element we report on ...
                      parent: TwistGroundRobot,
                      _topic: String,
                      rate: Double
                    ) extends Sensor(parent, _topic, rate)
{

  val mapFrame = "map"

  //from https://github.com/husky/husky_simulator/blob/hydro-devel/husky_gazebo_plugins/src/husky_plugin.cpp
  val pose_cov = Array[Double]( 1e-3, 0, 0, 0, 0, 0,
                                0, 1e-3, 0, 0, 0, 0,
                                0, 0, 1e6, 0, 0, 0,
                                0, 0, 0, 1e6, 0, 0,
                                0, 0, 0, 0, 1e6, 0,
                                0, 0, 0, 0, 0, 1e3)

  //todo add an sequence number and the time (global time from the World)
  val dummyHeader = Header(0, Time(0, 0), mapFrame)

  def mkOdometry2D(x: Double, y: Double, orientation: Double,
                   linSpeed: Double, angSpeed: Double) = {
    val pose = Pose2D(x, y, orientation)
    val position = PoseWithCovariance(Poses.from2To3D(pose), pose_cov)
    val speed = TwistWithCovariance(Twist(Vector3(linSpeed,0,0), Vector3(0,0,angSpeed)), pose_cov)
    // pose specified in the coordinate frame given by mapFrame
    // twist specified in the coordinate frame given by bodyFrame
    Odometry(dummyHeader, bodyFrame, position, speed)
  }
  
  def act {
    val msg = mkOdometry2D(parent.x, parent.y, parent.orientation,
                           parent.vx, parent.vo)
    val msg2 = exec.convertMessage[nav_msgs.Odometry](msg)
    exec.publish(topic, nav_msgs.Odometry._TYPE, msg2)
  }


}
