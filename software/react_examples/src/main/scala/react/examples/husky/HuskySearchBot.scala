package react.examples.husky

import java.util.Random
import react._
import react.robot._
import react.message._
import react.examples._


class HuskySearchBot(_id: String) extends GroundRobot(_id) with FsmController {

  def currentIntegralPosition = {
    val pX = math.round(x)
    val pY = math.round(y)
    val pO = Orientation.closest(orientation)
    (pX, pY, pO)
  }

  def modelName() = {
    _id.substring(1)
  }

  // def pickTarget() = {
  //   val dx = maxX - minX
  //   targetX = minX + new Random().nextInt(dx + 2)
  //   val dy = maxY - minY
  //   targetY = minY + new Random().nextInt(dy + 2)
  // }

  def clamp(v: Double, vMin: Double, vMax: Double) = math.min(vMax, math.max(v, vMin))

  def goStraightBy(d: Double) = {
    val (x, y, o) = currentIntegralPosition
    o match {
      case North => myX = x; myY = y + d; 
      case South => myX = x; myY = y - d; 
      case East  => myX = x + d; myY = y; 
      case West  => myX = x - d; myY = y; 
    }
    snap()
  }

  def rotate(rad: Double) = {
    val (x,y,o) = currentIntegralPosition
    myO = Angle.normalize(o.rad - rad)
    snap()
  }
  def turnRight() = rotate(-math.Pi / 2)
  def turnLeft()  = rotate(math.Pi / 2)

  def snap() = {
    publish("/gazebo/set_model_state", 
      Command.moveToAndOrient(modelName(), myX, myY, myO))
  }

  //update the position with the info from the robot
  sensor[Odometry]("p3d"){
    case GetPose(pX, pY, pT) =>
      x = pX
      y = pY
      orientation = pT
      if (currentState == 'init) {
        // pickTarget()
        nextState('main)
        println(" === switched to 'main state")
      }
  }

  var frontDistance = 1.0
  sensor[LaserScan]("laser"){
    case GetRange(distance) =>
      frontDistance = distance
      println("!!!!!!! laser info received: distance = " + distance)
  }

  val maxX = 10
  val minX = -10
  val maxY = 10
  val minY = -10;

  // var targetX = 0.0
  // var targetY = 0.0

  var myX = 0.0
  var myY = 0.0
  var myO = 0.0

  initialState('init)

  state('init) {
    println("@@@@@@ waiting to receive odometry data...")
  }

  state('main) {
    every(500) {
      if (frontDistance > 2.5) {
        val whatNext = new Random().nextInt(7)
        whatNext match {
          case 0 => turnLeft();        println("<--")
          case 1 => turnRight();       println("-->")
          case _ => goStraightBy(1.0); println("^^^")
        }        
      } else {
        turnLeft()
        println("---->")
      }
    }
  }

}
