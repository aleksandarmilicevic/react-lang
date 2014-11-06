package react.examples.arduino

import react._
import react.robot._
import react.message._
import react.examples._
import react.runtime.{Arduino, ArduinoExecutor}
import react.verification._
import react.verification.model._
import react.verification.environment._
import react.verification.ghost._

class FollowTheEdge(port: String, clockwise: Boolean = false) extends Robot(port) {

  val sensor = Arduino.D2
  val left = Arduino.D9
  val right = Arduino.D10

  var onTarget = false
  sensor[Primitive.Bool](sensor){
    case Primitive.Bool(b) =>
      //Console.println("onTarget ← " + b)
      onTarget = b
  }
  
  val speed: Short = 90
  every(100) {
    if (onTarget ^ clockwise) {
      //Console.println("turning right")
      publish(left, Primitive.Int16(speed))
      publish(right, Primitive.Int16(0))
    } else {
      //Console.println("turning left")
      publish(left, Primitive.Int16(0))
      publish(right, Primitive.Int16(speed))
    }
  }

}

//a verification scenario

class FollowTheEdgeTest extends World {
  
  val xMin = -3
  val xMax = 3
  val xDiscretization = 0.015625

  val yMin = -3
  val yMax = 3
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
  
  val i1 = "/robot1"
  val r1 = new FollowTheEdge(i1)
//val m1 = new TwoWheeledRobot(new Box2D(-0.5, -0.5, 0, 1, 1),
//                             i1, r1.left, r1.right,
//                             0.3, 1)
  val m1 = new TwoWheeledRobotWithError(new Box2D(-0.5, -0.5, 0, 1, 1),
                                        i1, r1.left, r1.right,
                                        0.3, 1, 0.5, 10)
  m1.setPosition(0,-1)
  val targets = List(
    new Box2D(-1,-1,0,2,2)
  )
  val gs = new GroundSensor( targets,
                             m1,
                             i1 + "/" + r1.sensor,
                             20,
                             false
                           )
  m1.addSensor(gs, Pose2D(0.5, 0, 0))
  robot(r1, m1)

}

object RunV {

  def apply(args: McOptions) {
    val world = (() => new FollowTheEdgeTest)
    val runner = new react.verification.McRunner(args, world)
    runner.run
  }

}
