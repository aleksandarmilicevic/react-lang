package react.verification.model

import react.message._
import react.verification.environment._

trait Positioned {

  def pose: Pose2D

  def pose_=(p: Pose2D)

  //def boundingBox: Box2D

}
