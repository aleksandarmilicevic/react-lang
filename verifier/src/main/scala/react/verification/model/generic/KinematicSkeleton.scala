package react.verification.model.generic

import react.message._
import dzufferey.smtlib._

object KinematicSkeleton {
  
  case class Vector(x: Formula, y: Formula, z: Formula)

  case class Quaternion(a: Formula, i: Formula, j: Formula, k: Formula)


  case class Frame(prefix: String) {
    // naming convention from the robot compiler
    def dx = Variable(prefix + ".dx").setType(Real)
    def dy = Variable(prefix + ".dy").setType(Real)
    def dz = Variable(prefix + ".dz").setType(Real)
    def q_a = Variable(prefix + ".q_a").setType(Real)
    def q_i = Variable(prefix + ".q_i").setType(Real)
    def q_j = Variable(prefix + ".q_j").setType(Real)
    def q_k = Variable(prefix + ".q_k").setType(Real)
    def position = Vector(dx, dy, dx)
    def orientation = Quaternion(q_a, q_i, q_j, q_k)
    // in ROS terms a frame is the symbolic version of:
    //  def position = Point(dx, dy, dx)
    //  def orientation = Quaternion(q_i, q_j, q_k, q_a) XXX ROS has a different convention for quaternions
    //  def pose = Pose(position, orientation)
  }

  sealed abstract class Link
  case class Fixed(j0: (Frame, Vector, Quaternion), j1: (Frame, Vector, Quaternion)) extends Link
  case class Revolute(j0: (Frame, Vector, Vector), j1: (Frame, Vector, Vector), alpha: Option[Formula]) extends Link
  case class Prismatic(j0: (Frame, Vector), j1: (Frame, Vector), direction: Vector, rotation: Quaternion, distance: Option[Formula]) extends Link

}

class KinematicSkeleton(frames: Seq[KinematicSkeleton.Frame], links: Seq[KinematicSkeleton.Link]) {
  def getEquations = ??? // TODO call maxima
}
