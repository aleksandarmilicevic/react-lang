package react.examples.husky

import react._
import react.robot._
import react.message._
import react.examples._
import react.examples.husky._

class HuskyGridSnap(_id: String) extends HuskyRobot(_id) with FsmController {

  initialState('init)

  state('init){

    every(100){
      val (x,y,o) = currentIntegralPosition
      targetX = x
      targetY = y
      targetO = o.rad
      nextState('loop)
    }

  }

  state('loop) {

    //input from keyboard
    on {
      case Key.UP =>
        val (x,y,o) = currentIntegralPosition
        if (frontDistance > 1.5) {
          println("move forward")
          o match {
            case North => targetX = x; targetY = y + 1
            case South => targetX = x; targetY = y - 1
            case East  => targetX = x + 1; targetY = y
            case West  => targetX = x - 1; targetY = y
          }
          snap()
          // nextState('moving)
        } else {
          println("too close")
        }
      case Key.LEFT => 
        val (x,y,o) = currentIntegralPosition
        targetO = Angle.normalize(o.rad + math.Pi / 2)
        println("turn left")
        snap()
        // nextState('turning)
      case Key.RIGHT =>
        val (x,y,o) = currentIntegralPosition
        targetO = Angle.normalize(o.rad - math.Pi / 2)
        println("turn right")
        snap()
        // nextState('turning)
    }
  }

  state('moving) {

    //moving the frame at the robot position
    // val dx = targetX - x
    // val dy = targetY - y
    // if (closeEnough(dx, dy)) {
    //   snap()
    //   nextState('loop)
    // } else {
    //     //normal to the line on which the robot currently moves
    //     val nx = math.sin(orientation)
    //     val ny = math.cos(orientation)
    //     //distance between T and the line
    //     val d = dx*nx + dy*ny
    //     //projection of T on the line
    //     val px = dx - d * nx
    //     val py = dy - d * ny
    //     val p = math.sqrt( px*px + py*py )
    //     //compute an arc to the next position
    //     val ang = -math.atan2(d, p)
    //     val sign = (px * ny + py * nx).signum
    //     val lin = sign * p / math.cos(ang)
    //     //scale down
    //     val rawScale = math.min(vMaxLinear / lin.abs, vMaxAngle / ang.abs)
    //     val scale = math.min(rawScale, 10)
    //     //
    //     // val cmd = Command.setSpeed(scale * lin, scale * ang)
    //     val cmd = Command.setSpeed(scale * lin, 0)
    //     publish("husky/cmd_vel", cmd)
    //     if(rawScale >= 10) {
    //       snap()
    //       // nextState('turning)
    //     }
    // }
  }


  // state('moving) {
  //   every(30){
  //     //moving the frame at the robot position
  //     val dx = targetX - x
  //     val dy = targetY - y
  //     if (closeEnough(dx, dy)) {
  //       snap()
  //       nextState('loop)
  //     } else {
  //       //normal to the line on which the robot currently moves
  //       val nx = math.sin(orientation)
  //       val ny = math.cos(orientation)
  //       //distance between T and the line
  //       val d = dx*nx + dy*ny
  //       //projection of T on the line
  //       val px = dx - d * nx
  //       val py = dy - d * ny
  //       val p = math.sqrt( px*px + py*py )
  //       //compute an arc to the next position
  //       val ang = -math.atan2(d, p)
  //       val sign = (px * ny + py * nx).signum
  //       val lin = sign * p / math.cos(ang)
  //       //scale down
  //       val rawScale = math.min(vMaxLinear / lin.abs, vMaxAngle / ang.abs)
  //       val scale = math.min(rawScale, 10)
  //       //
  //       // val cmd = Command.setSpeed(scale * lin, scale * ang)
  //       val cmd = Command.setSpeed(scale * lin, 0)
  //       publish("husky/cmd_vel", cmd)
  //       if(rawScale >= 10) {
  //         snap()
  //         // nextState('turning)
  //       }
  //     }
  //   }
  // }
  
  // state('turning) {
  //   every(100){
  //     val delta = Angle.normalize(targetO - orientation)
  //     val da = clamp(10 * delta, -vMaxAngle, vMaxAngle)
  //     val cmd = Command.setSpeed(0, da)
  //     publish("husky/cmd_vel", cmd)
  //     if(da < vMaxAngle && da > -vMaxAngle) {
  //       nextState('loop)
  //     }
  //   }
  // }

  // val vMaxAngle  = 1.0
  // val vMaxLinear = 1.0
  // def clamp(v: Double, vMin: Double, vMax: Double) = math.min(vMax, math.max(v, vMin))

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
