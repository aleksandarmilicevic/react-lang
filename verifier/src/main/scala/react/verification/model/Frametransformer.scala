package react.verification.model

import react.robot._
import react.message._
import react.verification._
import react.Executor
import math._

class FrameTransformer[A <: Positioned] extends Executed {

  @ignore var leafs: List[(A, Pose2D)] = Nil
  @ignore var nodes: List[(FrameTransformer[A], Pose2D)] = Nil
  
  def getLeafs: List[A] = leafs.map(_._1) ::: nodes.flatMap(_._1.getLeafs)

  def addLeaf(s: A, p: Pose2D) {
    leafs = (s,p) :: leafs
  }

  def addNode(s: FrameTransformer[A], p: Pose2D) {
    nodes = (s,p) :: nodes 
  }

  protected def getAbsolutePose(base: Pose2D, offset: Pose2D) = {
    val o = offsetTransform(offset)
    val cx = base.x + cos(base.theta) * o.x - sin(base.theta) * o.y
    val cy = base.y + sin(base.theta) * o.x + cos(base.theta) * o.y
    val co = Angle.normalize(base.theta + o.theta)
    Pose2D(cx, cy, co)
  }

  /* ID transformer: preserve the relative position. */
  protected def offsetTransform(p: Pose2D): Pose2D = p

  def updatePose(base: Pose2D) {
    for ( (s, p) <- leafs)
      s.pose = getAbsolutePose(base, p)
    for ( (s, p) <- nodes )
      s.updatePose( getAbsolutePose(base, p))
  }
  
  override def register(exec: Executor) {
    super.register(exec)
    nodes.foreach(_._1.register(exec))
  }
  
  override def deregister(exec: Executor) {
    super.deregister(exec)
    nodes.foreach(_._1.deregister(exec))
  }

}
