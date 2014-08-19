package react.robot

import react._
import react.message._

//some utils methods for the position

object Angle {

  def thetaFromQuaternion(q: Quaternion) = {
    // http://en.wikipedia.org/wiki/Conversion_between_quaternions_and_Euler_angles
    //math.atan2(2*(q.x*q.y + q.z*q.w), 1-2*(q.y*q.y + q.z*q.z))
    //math.asin(2*(q.x*q.z - q.y*q.w))
    //math.atan2(2*(q.x*q.w + q.y*q.z), 1-2*(q.z*q.z + q.w*q.w))
    math.atan2(2*(q.z*q.w), 1-2*(q.z*q.z))
  }

  def quaternionFromTheta(theta: Double) = {
    // http://en.wikipedia.org/wiki/Conversion_between_quaternions_and_Euler_angles
    //cos(phi /2) cos(theta /2) cos(psi /2) +  sin(phi /2) sin(theta /2) sin(psi /2)
    //sin(phi /2) cos(theta /2) cos(psi /2) -  cos(phi /2) sin(theta /2) sin(psi /2)
    //cos(phi /2) sin(theta /2) cos(psi /2) +  sin(phi /2) cos(theta /2) sin(psi /2)
    //cos(phi /2) cos(theta /2) sin(psi /2) -  sin(phi /2) sin(theta /2) cos(psi /2)
    val x = 0
    val y = 0
    val z = math.sin(theta/2)
    val w = math.cos(theta/2)
    Quaternion(x, y, z, w)
  }

  /** return an angle in [π,-π) */
  def normalize(angle: Double) = {
    val a2 = angle % (math.Pi * 2)
    if (a2 > math.Pi) {
      a2 - (math.Pi * 2)
    } else {
      a2
    }
  }


}

object GetPose {
  def unapply(m: Message): Option[(Double,Double,Double)] = m match {
      case Odometry(_, _, PoseWithCovariance(Pose(Point(x,y,_), q),_), _) =>
        Some((x, y, Angle.thetaFromQuaternion(q)))
      case Pose(Point(x,y,_), q) =>
        Some((x, y, Angle.thetaFromQuaternion(q)))
      case Pose2D(x, y, orientation) =>
        Some((x, y, orientation))
      case _ => None
  }
}


sealed abstract class Orientation(val deg: Double) {
  val rad = Angle.normalize(math.toRadians(deg))
}
case object East extends Orientation(0)
case object North extends Orientation(90)
case object West extends Orientation(180)
case object South extends Orientation(-90)

object Orientation {
  def closest(angle: Double) = {
    val secant1 =  math.Pi * 1 / 4 //  45
    val secant2 =  math.Pi * 3 / 4 // 135
    val secant3 = -math.Pi * 3 / 4 // 225
    val secant4 = -math.Pi * 1 / 4 // 315
    if ((angle >= 0 && angle < secant1) ||
        (angle <= 0 && angle >= secant4 ))         East
    else if (angle < secant2 && angle >= secant1)  North
    else if (angle < secant3 || angle >= secant2)  West
    else if (angle < secant4 && angle >= secant3)  South
    else sys.error("??!!")
  }
}
