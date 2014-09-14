package react.examples.husky

import react._
import react.robot._
import react.message._
import react.examples._

class HuskyRobot(_id: String) extends GroundRobot(_id) {

  def getEnv[T](key: String, defaultValue: T, parser: (String) => T): T = {
    val ans = System.getenv(key)
    if (ans != null)
      try {
        return parser(ans)
      } catch {
        case e: Exception => defaultValue
      }
    else
      return defaultValue
  }

  def getEnvI(key: String, value: Int): Int = getEnv(key, value, (s) => s.toInt)
  def getEnvD(key: String, v: Double): Double = getEnv(key, v, (s) => s.toDouble)

  val STAY_PUT    = -1
  val TURN_LEFT   = 0
  val TURN_RIGHT  = 1
  val GO_STRAIGHT = 2

  var targetX = 0.0
  var targetY = 0.0
  var targetO = 0.0

  val safeDistance    = getEnvD("REACT_SAFE_DISTANCE", 1.5)
  var frontDistance   = 1.0
  var poseUpdated     = false
  var distanceUpdated = false

  def currentIntegralPosition = {
    val pX = math.round(x)
    val pY = math.round(y)
    val pO = Orientation.closest(orientation)
    (pX, pY, pO)
  }

  def modelName() = {
    _id.substring(1)
  }

  def closeEnough(dx: Double, dy: Double) = {
    val xx = Math.abs(dx)
    val yy = Math.abs(dy)
    val d = Math.sqrt(xx*xx + yy+yy)
    d < 0.2
  }

  def snap() = {
    snapTo(targetX, targetY, targetO)
  }

  def snapTo(x: Double, y: Double, o: Double) = {
    publish("/gazebo/set_model_state", Command.moveToAndOrient(modelName(), x, y, o))
    this.x = x
    this.y = y
    this.orientation = o
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

  def rotate(rad: Double) = {
    val (x,y,o) = currentIntegralPosition
    val myO = Angle.normalize(o.rad - rad)
    snapTo(x, y, myO)
  }
  def turnRight() = rotate(-math.Pi / 2)
  def turnLeft()  = rotate(math.Pi / 2)

  def toOp(from: (Long,Long), to: (Long,Long)): Int = {
    val dx = to._1 - from._1
    val dy = to._2 - from._2
    assert(dx == 0 || dy == 0)
    
    def opAsIfNorth(dx: Long, dy: Long): Int = {
      if (dx == 0 && dy == 0) {
        println("  should stay")
        return STAY_PUT
      } else if (dx == 0) {
        if (dy > 0) {
          println("  should go straight")
          GO_STRAIGHT
        } else {
          println("  should go back")
          TURN_LEFT
        }
      } else if (dx < 0) {
          println("  should turn left")
          TURN_LEFT
      } else {
          println("  should turn right")
          TURN_RIGHT
      }
    }
    
    currentIntegralPosition._3 match {
      case North => 
        println("pos: North")
        opAsIfNorth(dx, dy)
      case South => 
        println("pos: South")
        opAsIfNorth(-dx, -dy)
        // if (dx == 0)
        //   if (dy < 0) 
        //     GO_STRAIGHT 
        //   else 
        //     TURN_LEFT
        // else if (dx > 0) 
        //   TURN_LEFT
        // else
        //   TURN_RIGHT
      case East  => 
        println("pos: East")
        opAsIfNorth(-dy, dx)
        // if (dy == 0)
        //   if (dx > 0) 
        //     GO_STRAIGHT 
        //   else 
        //     TURN_LEFT
        // else if (dy > 0) 
        //   TURN_LEFT
        // else
        //   TURN_RIGHT
      case West  => 
        println("pos: West")
        opAsIfNorth(-dy, -dx)
        // if (dy == 0)
        //   if (dx < 0) 
        //     GO_STRAIGHT 
        //   else 
        //     TURN_LEFT
        // else if (dy > 0) 
        //   TURN_LEFT
        // else
        //   TURN_RIGHT
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
      // println("laser: distance = " + frontDistance)
  }


}