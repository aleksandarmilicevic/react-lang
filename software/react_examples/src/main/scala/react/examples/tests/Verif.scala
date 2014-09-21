package react.examples.tests

import react._
import react.robot._
import react.message._
import react.verification._
import react.verification.model._
import react.verification.environment._
import react.verification.ghost._

class Verif extends World {

  val xMin = -3
  val xMax = 3
  val xDiscretization = 0.2

  val yMin = -3
  val yMax = 3
  val yDiscretization = 0.2

  val enclosed = true
  
  val fpDiscretization = 0.1

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
  
  def twistModel(id: String, x: Int, y: Int, odo: Boolean = false) = {
    val sensorFreq = 20

    val m1 = new TwistGroundRobot(new Box2D(-0.4, -0.4, 0, 0.8, 0.8),
                                  "/" + id + "/cmd_vel",
                                  100,
                                  Some(("/gazebo/set_model_state", id)))
    m1.setPosition(x, y)

    //val ls1 = new LaserSensor(-0.2f, 0.2f, 90, 0.1f, 5, 0.01f, m1, "/" + id + "/laser", sensorFreq)
    val ls1 = new LaserSensor(-0.78f, 0.78f, 90, 0.1f, 5, 0.01f, m1, "/" + id + "/laser", sensorFreq)
    m1.addSensor(ls1, Pose2D(0,0,0))

    if (odo) {
      val os1 = new OdometrySensor(id, m1, "/" + id + "/p3d", sensorFreq)
      m1.addSensor(os1, Pose2D(0,0,0))
    }

    m1
  }

  val id = "robot1"
  val r1 = new Snappy("/" + id)
  val m1 = twistModel(id, 0, 0, true) 

  robot(r1, m1)
  ghost(new UserInput(r1))

}

class RunVerif extends McExecutor {
  val world = new Verif
  override def getMcOptions = react.examples.Main
}
  
