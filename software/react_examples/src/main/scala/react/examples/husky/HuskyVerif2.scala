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

  def huskyModel(id: String, x: Int, y: Int) = {
    val sensorFreq = 20
    val m1 = new GroundRobot(new Box2D(-0.4, -0.4, 0, 0.8, 0.8), Some(("/gazebo/set_model_state", id)))
    //val ls1 = new LaserSensor(-0.2f, 0.2f, 90, 0.1f, 5, 0.01f, m1, "/" + id + "/laser", sensorFreq)
    val ls1 = new LaserSensor(-0.78f, 0.78f, 90, 0.1f, 5, 0.01f, m1, "/" + id + "/laser", sensorFreq)
    val os1 = new OdometrySensor(id, m1, "/" + id + "/p3d", sensorFreq)
    m1.setPosition(x, y)
    m1.addSensor(ls1, Pose2D(0,0,0))
    m1.addSensor(os1, Pose2D(0,0,0))
    m1
  }

  def addPathfinder(id: String, x: Int, y: Int) = {
    val husky1 = new HuskyPathfinder("/" + id)
    val model = huskyModel(id, x, y)
    robot(husky1, model)
  }

  def addRandom(id: String, x: Int, y: Int) = {
    val husky1 = new HuskySearchBot("/" + id)
    val model = huskyModel(id, x, y)
    robot(husky1, model)
  }
  
  def addSnap(id: String, x: Int, y: Int) = {
    val husky1 = new HuskyGridSnap("/" + id)
    val model = huskyModel(id, x, y)
    robot(husky1, model)
    ghost(new UserInput(husky1))
  }

  addSnap("husky1", 0, 0)
  
  //addSnap("husky2", 1, -1)


}

class RunHuskyVerif2 extends McExecutor {
  val world = new HuskyVerif2
  override def getMcOptions = react.examples.Main
}
  
