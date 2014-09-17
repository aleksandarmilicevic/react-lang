package react.examples.husky

import react.message._
import react.verification._
import react.verification.model._
import react.verification.environment._
import react.verification.ghost._

class HuskyVerif2 extends World {

  val xMin = -5
  val xMax = 5
  val xDiscretization = 1.0

  val yMin = -10
  val yMax = 5
  val yDiscretization = 1.0

  val enclosed = true
  
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

  def addHuskyPathfinder(id: String, x: Int, y: Int) = {
    val husky1 = new HuskyPathfinder("/" + id)
    val m1 = new GroundRobot(new Box2D(-0.5, -0.5, 0, 1, 1), Some(("/gazebo/set_model_state", id)))
    val ls1 = new LaserSensor(-0.78f, 0.78f, 90, 0.1f, 5, 0.01f, m1, "/" + id + "/laser", 20)
    val os1 = new OdometrySensor(id, m1, "/" + id + "/p3d", 20)
    m1.setPosition(x, y)
    m1.addSensor(ls1, Pose2D(0,0,0))
    m1.addSensor(os1, Pose2D(0,0,0))
    robot(husky1, m1)
  }

  addHuskyPathfinder("husky1", 0, 0)
  
  addHuskyPathfinder("husky2", -2, 3)


}

class RunHuskyVerif2 extends McExecutor {
  val world = new HuskyVerif2
  override def getMcOptions = react.examples.Main
}
  
