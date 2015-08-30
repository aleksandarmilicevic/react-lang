package react.examples.arduino

import react.message._
import react.verification._
import react.verification.model._
import react.verification.environment._
import react.verification.ghost._

abstract class VerifTemplate extends World {

  val xMin = -1
  val xMax = 1
  //val xDiscretization = 0.015625 // 4 //4
  val xDiscretization = 2.0 / 100

  val yMin = -1
  val yMax = 1
  //val yDiscretization = 0.015625 // 4 //4
  val yDiscretization = 2.0 / 100

  val enclosed = true
  
  //val fpDiscretization = 0.015625
  //val fpDiscretization = math.Pi / 36
  val fpDiscretization = math.Pi / 72
  //val fpDiscretization = math.Pi / 144
      

  /////////
  def noCollision = {
    val bxs = allBoxes
    bxs.forall( b =>
      models.forall( r => {
        val br = r.boundingBox
        var res = (b == br) || !(b collides br)
        //if (!res) println("collides: " + b + " and " + br)
        res
      })
    )
  }

  // Edge
  //def safe = true 
  //obstacle(new Box2D( 0.0,   0.0,    0.0 , 0.25,  0.05))
  //obstacle(new Box2D( 0.25,  0.0,   0.3 , 0.1,   0.05))
  //obstacle(new Box2D( 0.015, 0.0476, 2.83, 0.125, 0.05))

  // SwipeScan
  def safe = noCollision
  
  val bodyLength = 0.12
  val bodyWidth = 0.09
  val wheelRadius = 0.03


  def mkModel(id: String, left: String, right: String,
              x: Double, y: Double, o: Double) = {

  val m1 = new TwoWheeledRobot(new Box2D(-bodyLength/2, -bodyWidth/2, 0, bodyLength, bodyWidth),
                               id, left, right,
                               wheelRadius, bodyWidth)
  //val m1 = new TwoWheeledRobotWithError(new Box2D(-bodyLength/2, -bodyWidth/2, 0, bodyLength, bodyWidth),
  //                                      id, left, right,
  //                                      wheelRadius, bodyWidth,
  //                                      0.2, 10)
    m1.setPosition(x,y)
    m1.setOrientation(o)
    m1
  }

  def addSensorTarget(m: GroundRobot, id: String, topic: String) {
    val targets = List(
      new Box2D( 0.0,   0.0,    0.0 , 0.25,  0.05),
      new Box2D( 0.25,  0.0,    0.3 , 0.1,   0.05),
      new Box2D( 0.015, 0.0476, 2.83, 0.125, 0.05)
    )
    val gs = new GroundSensor( targets,
                               m,
                               id + "/" + topic,
                               20,
                               false
                             )
    m.addSensor(gs, Pose2D(0.05, 0, 0))
    //m.addSensor(gs, Pose2D(-0.05, 0, 0)) //sensor on the back
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
                   0, 0, 0)
  addSwipeSensor(m1, i1, r1.sensorDist, r1.sensorServo)
  robot(r1, m1)

  val i2 = "/robot2"
  val r2 = new SwipeScan(i2)
  val m2 = mkModel(i2, r2.motorLeft, r2.motorRight,
                   0, 0.5, 0)
  addSwipeSensor(m2, i2, r2.sensorDist, r2.sensorServo)
  robot(r2, m2)
}

class FollowTheEdgeTest extends VerifTemplate {
  
  val i1 = "/robot1"
  val r1 = new FollowTheEdge(i1)
  val m1 = mkModel(i1, r1.servoLeft, r1.servoRight,
                   0, -0.01, 0)
  addSensorTarget(m1, i1, r1.sensor)
  robot(r1, m1)

}

class RightHandRuleTest extends VerifTemplate {
  
  val i1 = "/robot1"
  val r1 = new RightHandRule(i1)
  val m1 = mkModel(i1, r1.servoLeft, r1.servoRight,
                   0, -0.5, 0)
  addSensorDistance(m1, i1, r1.sensorDist)
  robot(r1, m1)

  val i2 = "/robot2"
  val r2 = new RightHandRule(i2)
  val m2 = mkModel(i2, r2.servoLeft, r2.servoRight,
                   0, 0.5, 0)
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
                   0, 0.5, 0)
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
