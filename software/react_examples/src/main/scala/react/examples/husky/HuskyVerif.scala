package react.examples.husky

import react._
import react.robot._
import react.message._
import react.verification._
import react.verification.model._
import react.verification.environment._
import react.verification.ghost._

class HuskyVerif extends World {

  val xMin = -3
  val xMax = 3
  val xDiscretization = 1.0

  val yMin = -3
  val yMax = 3
  val yDiscretization = 1.0

  val enclosed = true
  
  /////////

  def safe = {
    val bxs = allBoxes
    bxs.forall( x =>
      bxs.forall( y =>
        (x == y) || !(x collides y)
      )
    )
  }

  obstacle( new Box2D(1, 2, 0, 2, 1) )

  /////////

  val husky1 = new HuskyGrid("husky1")
  val m1 = new TwistGroundRobot(new Box2D(-0.5, -0.5, 0, 1, 1),
                                "/husky1/cmd_vel",
                                100)
  val ls1 = new LaserSensor(-0.78f, 0.78f, 90, 0.1f, 5, 0.01f, m1, "/husky1/laser", 30)
  val os1 = new OdometrySensor("husky1", m1, "/husky1/p3d", 30)
  m1.addSensor(ls1, Pose2D(0,0,0))
  m1.addSensor(os1, Pose2D(0,0,0))

  robot(husky1, m1)

  ghost(new UserInput(husky1))

}

class HuskyVerifExec extends McExecutor {
  val world = new HuskyVerif
}
  
object HuskyVerifRunner { 
  def main(args: Array[String]) {
    org.ros.RosRun.main(Array(classOf[HuskyVerifExec].getName))
  }
}

