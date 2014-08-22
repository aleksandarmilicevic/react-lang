package react.verification.ghost

import react._
import react.verification._
import react.message._
import react.robot._

class OdometrySensor( bodyFrame: String, //the element we report on ...
                      topic: String,
                      rate: Double
                    ) extends Sensor(topic, rate)
{

  val mapFrame = "map"


  //from https://github.com/husky/husky_simulator/blob/hydro-devel/husky_gazebo_plugins/src/husky_plugin.cpp
  val pose_cov = Array[Double]( 1e-3, 0, 0, 0, 0, 0,
                                0, 1e-3, 0, 0, 0, 0,
                                0, 0, 1e6, 0, 0, 0,
                                0, 0, 0, 1e6, 0, 0,
                                0, 0, 0, 0, 1e6, 0,
                                0, 0, 0, 0, 0, 1e3)


  //TODO how to access the thing we report on ?


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


}
