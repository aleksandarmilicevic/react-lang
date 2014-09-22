package react.examples.tests

import react._
import react.robot._
import react.message._
import react.verification._
import react.verification.model._
import react.verification.environment._
import react.verification.ghost._

abstract class VerifTemplate extends World {
  
  val xMin = -3
  val xMax = 3
  val xDiscretization = 0.2

  val yMin = -3
  val yMax = 3
  val yDiscretization = 0.2

  val enclosed = true
  
  val fpDiscretization = 0.1

  /////////

  obstacle( new Box2D(1, 2, 0, 2, 1) )

  def noCollision = {
    val bxs = allBoxes
    bxs.forall( b =>
      models.forall( r => {
        val br = r.boundingBox
        (b == br) || !(b collides br)
      })
    )
  }

  def twistModel(id: String, x: Int, y: Int, odo: Boolean = false, angle: Float = 0.78f) = {
    val sensorFreq = 20

    val m1 = new TwistGroundRobot(new Box2D(-0.4, -0.4, 0, 0.8, 0.8),
                                  "/" + id + "/cmd_vel",
                                  100,
                                  Some(("/gazebo/set_model_state", id)))
    m1.setPosition(x, y)

    if (angle > 0) {
      val ls1 = new LaserSensor(-angle, angle, 90, 0.1f, 5, 0.01f, m1, "/" + id + "/laser", sensorFreq)
      m1.addSensor(ls1, Pose2D(0,0,0))
    }

    if (odo) {
      val os1 = new OdometrySensor(id, m1, "/" + id + "/p3d", sensorFreq)
      m1.addSensor(os1, Pose2D(0,0,0))
    }

    m1
  }

}

class Verif0 extends VerifTemplate {

  def safe = consistent && noCollision

  def consistent = (!r1.poseUpdated || (r1.x == m1.x && r1.y == m1.y))

  val i1 = "robot1"
  val r1 = new Snappy("/" + i1)
  val m1 = twistModel(i1, 0, 0, true) 
  robot(r1, m1)
  ghost(new UserInput(r1))

}

class Verif1s extends VerifTemplate {

  def safe = consistent && noCollision

  def consistent = {
    (!r1.poseUpdated || (r1.x == m1.x && r1.y == m1.y)) &&
    (!r2.poseUpdated || (r2.x == m2.x && r2.y == m2.y))
  }

  /////////
  
  val i1 = "robot1"
  val r1 = new Snappy("/" + i1)
  val m1 = twistModel(i1, 0, 0, true) 
  robot(r1, m1)
  ghost(new UserInput(r1))

  val i2 = "robot2"
  val r2 = new Snappy("/" + i2)
  val m2 = twistModel(i2, 1, -2, true) 
  robot(r2, m2)
  ghost(new UserInput(r2))

}

class Verif1u extends VerifTemplate {

  def safe = consistent && noCollision

  def consistent = {
    (!r1.poseUpdated || (r1.x == m1.x && r1.y == m1.y)) &&
    (!r2.poseUpdated || (r2.x == m2.x && r2.y == m2.y))
  }

  /////////
  
  val i1 = "robot1"
  val r1 = new Snappy("/" + i1)
  val m1 = twistModel(i1, 0, 0, true, 0.2f) 
  robot(r1, m1)
  ghost(new UserInput(r1))

  val i2 = "robot2"
  val r2 = new Snappy("/" + i2)
  val m2 = twistModel(i2, 1, -2, true, 0.2f) 
  robot(r2, m2)
  ghost(new UserInput(r2))

}

class Verif2 extends VerifTemplate {

  def safe = noCollision

  val i1 = "robot1"
  val r1 = new Square("/" + i1)
  val m1 = twistModel(i1, 0, 0, true) 
  robot(r1, m1)

}

class Verif3 extends VerifTemplate {

  def safe = noCollision

  val i1 = "robot1"
  val r1 = new Griddy("/" + i1)
  val m1 = twistModel(i1, 0, 0, true) 
  robot(r1, m1)
  ghost(new UserInput(r1))

}

class Verif4u extends VerifTemplate {
  
  def safe = noCollision

  val i1 = "robot1"
  val r1 = new Griddy("/" + i1, 1.5f)
  val m1 = twistModel(i1, 0, 0, true) 
  robot(r1, m1)
  ghost(new UserInput(r1))

  val i2 = "robot2"
  val r2 = new Griddy("/" + i2, 1.5f)
  val m2 = twistModel(i2, 2, 0, true) 
  m2.setOrientation(West.rad)
  robot(r2, m2)
  ghost(new UserInput(r2))

}

class Verif4s extends VerifTemplate {
  
  def safe = noCollision

  val i1 = "robot1"
  val r1 = new Griddy("/" + i1, 2.6f)
  val m1 = twistModel(i1, 0, 0, true) 
  robot(r1, m1)
  ghost(new UserInput(r1))

  val i2 = "robot2"
  val r2 = new Griddy("/" + i2, 2.6f)
  val m2 = twistModel(i2, 2, 0, true) 
  m2.setOrientation(West.rad)
  robot(r2, m2)
  ghost(new UserInput(r2))

}

class RunVerif(what: String) extends McExecutor {
  val world = what match {
    case "0" => new Verif0
    case "1s" => new Verif1s
    case "1u" => new Verif1u
    case "2" => new Verif2
    case "3" => new Verif3
    case "4s" => new Verif4s
    case "4u" => new Verif4u
    case _ => new Verif0
  }
  override def getMcOptions = react.examples.Main
}
  
