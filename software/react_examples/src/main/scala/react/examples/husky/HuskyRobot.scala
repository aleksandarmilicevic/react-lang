package react.examples.husky

import react._
import react.robot._
import react.message._
import react.utils.Env
import react.examples._

class HuskyRobot(_id: String) extends GroundRobot(_id) {

  val STAY_PUT    = -1
  val TURN_LEFT   = 0
  val TURN_RIGHT  = 1
  val GO_STRAIGHT = 2

  var targetX = 0.0
  var targetY = 0.0
  var targetO = 0.0

  val safeDistance    = Env.getDouble("REACT_SAFE_DISTANCE", 2.7)
  var frontDistance   = 1.0
  var poseUpdated     = false
  var distanceUpdated = false

  def modelName() = {
    _id.substring(1)
  }

  def closeEnough(dx: Double, dy: Double) = {
    val xx = Math.abs(dx)
    val yy = Math.abs(dy)
    val d = Math.sqrt(xx*xx + yy*yy)
    d < 0.2
  }

  def snap() = {
    snapTo(targetX, targetY, targetO)
  }

  def snapTo(x: Double, y: Double, o: Double, invalidateDistanceUpdated: Boolean = true) = {
    publish("/gazebo/set_model_state", Command.moveToAndOrient(modelName(), x, y, o))
    this.x = x
    this.y = y
    this.orientation = o
    if (invalidateDistanceUpdated)
      distanceUpdated = false
  }

  def goStraightBy(d: Int) = {
    var (x, y, o) = currentIntegralPosition
    o match {
      case North => y += d
      case South => y -= d
      case East  => x += d
      case West  => x -= d
    }
    snapTo(x, y, orientation)
  }

  def rotateTo(rad: Double) = {
    val (x,y,_) = currentIntegralPosition
    val myO = Angle.normalize(rad)
    snapTo(x, y, myO, false)
    myO
  }
  def rotate(rad: Double) = {
    val (x,y,o) = currentIntegralPosition
    val myO = Angle.normalize(o.rad + rad)
    snapTo(x, y, myO, false)
    myO
  }
  def turnRight() = rotate(-math.Pi / 2)
  def turnLeft()  = rotate(math.Pi / 2)

  def toOps(from: (Int,Int), to: (Int,Int)): List[Int] = {
    val dx = to._1 - from._1
    val dy = to._2 - from._2
    assert(dx == 0 || dy == 0)

    //println("from: " + from)
    //println("to: " + to)
    
    def opsAsIfNorth(dx: Int, dy: Int): List[Int] = {
      if (dx == 0 && dy == 0) {
        //println("  should stay")
        List(STAY_PUT)
      } else if (dx == 0) {
        if (dy > 0) {
          //println("  should go straight")
          List(GO_STRAIGHT)
        } else {
          //println("  should go back")
          List(TURN_LEFT, TURN_LEFT, GO_STRAIGHT)
        }
      } else if (dx < 0) {
          //println("  should turn left")
          List(TURN_LEFT, GO_STRAIGHT)
      } else {
          //println("  should turn right")
          List(TURN_RIGHT, GO_STRAIGHT)
      }
    }
    
    currentIntegralPosition._3 match {
      case North => opsAsIfNorth(dx, dy)
      case South => opsAsIfNorth(-dx, -dy)
      case East  => opsAsIfNorth(-dy, dx)
      case West  => opsAsIfNorth(dy, -dx)
    }
  }

  //update the position with the info from the robot
  sensor[Odometry]("p3d"){
    case GetPose(pX, pY, pT) =>
      x = pX
      y = pY
      orientation = pT
      poseUpdated = true
  }

  sensor[LaserScan]("laser"){
    case GetRange(distance) =>
      frontDistance = distance
      distanceUpdated = true
      //println("laser: distance = " + frontDistance)
  }


}
