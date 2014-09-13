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
      println("laser: distance = " + frontDistance)
  }


}
