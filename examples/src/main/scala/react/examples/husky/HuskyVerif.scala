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
  val xDiscretization = 0.2

  val yMin = -3
  val yMax = 3
  val yDiscretization = 0.2

  val enclosed = true
  
  val fpDiscretization = 0.05

  /////////

  def safe = {
    val bxs = allBoxes
    bxs.forall( b =>
      models.forall( r => {
        val br = r.boundingBox
        (b == br) || !(b collides br)
      })
    )
  }

  obstacle( new Box2D(1, 2, 0, 2, 1) )

  /////////

  //val husky1 = new HuskyTeleop("/husky1")
  val husky1 = new HuskyGrid("/husky1")
  val m1 = new TwistGroundRobot(new Box2D(-0.5, -0.5, 0, 1, 1),
                                "/husky1/husky/cmd_vel",
                                100)
  val ls1 = new LaserSensor(-0.2f, 0.2f, 90, 0.1f, 5, 0.01f, m1, "/husky1/laser", 20)
  //val ls1 = new LaserSensor(-0.78f, 0.78f, 90, 0.1f, 5, 0.01f, m1, "/husky1/laser", 20)
  val os1 = new OdometrySensor("husky1", m1, "/husky1/p3d", 20)
  m1.addSensor(ls1, Pose2D(0,0,0))
  m1.addSensor(os1, Pose2D(0,0,0))

  robot(husky1, m1)

  ghost(new UserInput(husky1))

}

