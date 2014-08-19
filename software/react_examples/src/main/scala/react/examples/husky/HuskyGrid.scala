package react.examples.husky

import react._
import react.robot._
import react.message._
import react.examples._


class HuskyGrid(_id: String) extends GroundRobot(_id) {

  def currentIntegralPosition = {
    val pX = math.round(x)
    val pY = math.round(y)
    val pO = Orientation.closest(orientation)
    (pX, pY, pO)
  }

  //update the position with the info from the robot
  sensor[Odometry]("p3d"){
    case GetPose( pX, pY, pT) =>
      x = pX
      y = pY
      orientation = pT
  }

  var frontDistance = 1.0
  sensor[LaserScan]("laser"){
    case GetRange(distance) =>
      frontDistance = distance
  }


  def decideNextTarget = {
    val (x,y,o) = currentIntegralPosition
    (scala.util.Random.nextInt % 4) match {
      case 0 =>
        targetO = Angle.normalize(o.rad + math.Pi / 2)
        state = sOrientation
        println("turn left")
      case 1 =>
        targetO = Angle.normalize(o.rad - math.Pi / 2)
        state = sOrientation
        println("turn right")
      case _ =>
        if (frontDistance > 1.5) {
          o match {
            case North => targetX = x; targetY = y + 1
            case South => targetX = x; targetY = y - 1
            case East  => targetX = x + 1; targetY = y
            case West  => targetX = x - 1; targetY = y
          }
          state = sMove
          println("move forward")
        } else {
          println("too close")
        }
    }
  }

  //input from keyboard
  on {
    case Key.UP if state == sNone =>
        val (x,y,o) = currentIntegralPosition
        if (frontDistance > 1.5) {
          o match {
            case North => targetX = x; targetY = y + 1
            case South => targetX = x; targetY = y - 1
            case East  => targetX = x + 1; targetY = y
            case West  => targetX = x - 1; targetY = y
          }
          state = sMove
          println("move forward")
        } else {
          println("too close")
        }
    case Key.LEFT if state == sNone => 
        val (x,y,o) = currentIntegralPosition
        targetO = Angle.normalize(o.rad + math.Pi / 2)
        state = sOrientation
        println("turn left")
    case Key.RIGHT if state == sNone =>
        val (x,y,o) = currentIntegralPosition
        targetO = Angle.normalize(o.rad - math.Pi / 2)
        state = sOrientation
        println("turn right")
 }

  val sNone = 0
  val sMove = 1
  val sOrientation = 2
  var state = sNone

  val vMaxAngle  = 1.0
  val vMaxLinear = 1.0
  def clamp(v: Double, vMin: Double, vMax: Double) = math.min(vMax, math.max(v, vMin))

  def moveTowardTarget = {
    if (state == sMove) {
      //moving the frame at the robot position
      val dx = targetX - x
      val dy = targetY - y
      //normal to the line on which the robot currently moves
      val nx = math.sin(orientation)
      val ny = math.cos(orientation)
      //distance between T and the line
      val d = dx*nx + dy*ny 
      //projection of T on the line
      val px = dx - d * nx
      val py = dy - d * ny
      val p = math.sqrt( px*px + py*py )
      //compute an arc to the next position
      val ang = -math.atan2(d, p)
      val sign = (px * ny + py * nx).signum
      val lin = sign * p / math.cos(ang)
      //scale down
      val rawScale = math.min(vMaxLinear / lin.abs, vMaxAngle / ang.abs)
      val scale = math.min(rawScale, 10)
      //
      val cmd = Command.setSpeed(scale * lin, scale * ang)
      publish("husky/cmd_vel", cmd)
      if(rawScale >= 10) {
        state = sOrientation
      }

    } else if (state == sOrientation) {
      val delta = Angle.normalize(targetO - orientation)
      val da = clamp(10 * delta, -vMaxAngle, vMaxAngle)
      val cmd = Command.setSpeed(0, da)
      publish("husky/cmd_vel", cmd)
      if(da < vMaxAngle && da > -vMaxAngle) {
        state = sNone
      }
    }
  }

  var init = true
  var targetX = 0.0
  var targetY = 0.0
  var targetO = 0.0

  //husky expect a message every 100ms
  every(100){
    //the first time: initialize the robot position
    if(init) {
      init = false
      val (x,y,o) = currentIntegralPosition
      targetX = x
      targetY = y
      targetO = o.rad
    }
    //close enough to target, decide what to do next
//  if(state == sNone) {
//    decideNextTarget
//  }
    //move toward target
    moveTowardTarget
  }
  
////print some debug every 5 second
//every(5000){
//  println("info:")
//  println("  x: " + x)
//  println("  y: " + y)
//  println("  θ: " + orientation)
//  val (_x,_y,_o) = currentIntegralPosition
//  println(" integral:")
//  println("  x: " + _x)
//  println("  y: " + _y)
//  println("  θ: " + _o)
//}

}
