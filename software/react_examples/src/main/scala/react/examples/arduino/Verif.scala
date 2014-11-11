package react.examples.arduino

import react.message._
import react.verification._
import react.verification.model._
import react.verification.environment._
import react.verification.ghost._

abstract class VerifTemplate extends World {

  val xMin = -1
  val xMax = 1
  val xDiscretization = 0.015625

  val yMin = -1
  val yMax = 1
  val yDiscretization = 0.015625

  val enclosed = true
  
  val fpDiscretization = 0.015625

  /////////
  def noCollision = {
    val bxs = allBoxes
    bxs.forall( b =>
      models.forall( r => {
        val br = r.boundingBox
        (b == br) || !(b collides br)
      })
    )
  }

  def safe = noCollision

  def mkModel(id: String, left: String, right: String,
              x: Double, y: Double, o: Double,
              wheelRadius: Double, wheelSeparation: Double) = {

  val m1 = new TwoWheeledRobot(new Box2D(-0.1, -wheelSeparation/2, 0, 0.2, wheelSeparation),
                               id, left, right,
                               wheelRadius, wheelSeparation)
  //val m1 = new TwoWheeledRobotWithError(new Box2D(-0.1, -wheelSeparation/2, 0, 0.2, wheelSeparation),
  //                                      id, left, right,
  //                                      wheelRadius, wheelSeparation,
  //                                      0.2, 10)
    m1.setPosition(x,y)
    m1.setOrientation(o)
    m1
  }

  def addSensorTarget(m: GroundRobot, id: String, topic: String) {
    val targets = List(
      new Box2D(-0.5,-0.5,0,1,1)
    )
    val gs = new GroundSensor( targets,
                               m,
                               id + "/" + topic,
                               20,
                               false
                             )
    m.addSensor(gs, Pose2D(0.1, 0, 0))
  }
  
  def addSensorDistance(m: GroundRobot, id: String, topic: String) {
    val ds = new IrSensor(  m,
                            id + "/" + topic,
                            20,
                            3
                         )
    m.addSensor(ds, Pose2D(0.1, 0, 0))
  }
  
  def addSwipeSensor(m: GroundRobot, id: String, sensorTopic: String, servoTopic: String) {
    val ds = new IrSensor(  m,
                            id + "/" + sensorTopic,
                            20,
                            3
                         )
    val servo = new Servo[Sensor](id + "/" + servoTopic)
    m.addNode(servo, Pose2D(0.1, 0, 0))
    servo.addLeaf(ds, Pose2D(0, 0, 0))
    stateful(servo)
  }
  
}

class SwipeScanTest extends VerifTemplate {
  
  val i1 = "/robot1"
  val r1 = new SwipeScan(i1)
  val m1 = mkModel(i1, r1.motorLeft, r1.motorRight,
                   0, -0.5, 0,
                   0.1, 0.2)
  addSwipeSensor(m1, i1, r1.sensorDist, r1.sensorServo)
  robot(r1, m1)

  val i2 = "/robot2"
  val r2 = new SwipeScan(i2)
  val m2 = mkModel(i2, r2.motorLeft, r2.motorRight,
                   0, 0.5, 0,
                   0.1, 0.2)
  addSwipeSensor(m2, i2, r2.sensorDist, r2.sensorServo)
  robot(r2, m2)
}

class FollowTheEdgeTest extends VerifTemplate {
  
  val i1 = "/robot1"
  val r1 = new FollowTheEdge(i1)
  val m1 = mkModel(i1, r1.servoLeft, r1.servoRight,
                   0, -0.5, 0,
                   0.1, 0.2)
  addSensorTarget(m1, i1, r1.sensor)
  robot(r1, m1)

}

class RightHandRuleTest extends VerifTemplate {
  
  val i1 = "/robot1"
  val r1 = new RightHandRule(i1)
  val m1 = mkModel(i1, r1.servoLeft, r1.servoRight,
                   0, -0.5, 0,
                   0.1, 0.2)
  addSensorDistance(m1, i1, r1.sensorDist)
  robot(r1, m1)

  val i2 = "/robot2"
  val r2 = new RightHandRule(i2)
  val m2 = mkModel(i2, r2.servoLeft, r2.servoRight,
                   0, 0.5, 0,
                   0.1, 0.2)
  addSensorDistance(m2, i2, r2.sensorDist)
  robot(r2, m2)

}

class RightHandRuleAndUserTest extends VerifTemplate {
  
//val i1 = "/robot1"
//val r1 = new RightHandRule(i1)
//val m1 = mkModel(i1, r1.servoLeft, r1.servoRight,
//                 0, -0.5, 0,
//                 0.1, 0.2)
//addSensorDistance(m1, i1, r1.sensorDist)
//robot(r1, m1)

  val i2 = "/robot2"
  val r2 = new UserControlled(i2)
  val m2 = mkModel(i2, r2.servoLeft, r2.servoRight,
                   0, 0.5, 0,
                   0.1, 0.2)
  addSensorDistance(m2, i2, r2.sensorDist)
  robot(r2, m2)
  ghost(new UserInput(r2))

}

object RunV {

  def apply(test: String, args: McOptions) {
    val world = test match {
      case "rhr" => (() => new RightHandRuleTest)
      case "ru" => (() => new RightHandRuleAndUserTest)
      case "s" => (() => new SwipeScanTest)
      case _ => (() => new FollowTheEdgeTest)
    }
    val runner = new react.verification.McRunner(args, world)
    runner.run
  }

}
