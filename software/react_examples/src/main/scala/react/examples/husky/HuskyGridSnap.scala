package react.examples.husky

import react._
import react.robot._
import react.message._
import react.examples._
import react.examples.husky._

class HuskyGridSnap(_id: String) extends HuskyRobot(_id) with FsmController {

  val tmpWait = getEnvI("REACT_STEP_WAIT", 5)
  val steps = getEnvI("REACT_STEPS", 20)
  var stepCnt = 0
  var dX = 0.0
  var dY = 0.0
  var dO = 0.0

  initialState('init)

  state('init) {
    every(100){
      if (poseUpdated) {
        val (x,y,o) = currentIntegralPosition
        targetX = x
        targetY = y
        targetO = o.rad
        nextState('loop)
      }
    }
  }

  state('loop) {
    //input from keyboard
    on {
      case Key.UP =>
        val (x,y,o) = currentIntegralPosition
        if (distanceUpdated && frontDistance > safeDistance) {
          o match {
            case North => targetX = x; targetY = y + 1; 
            case South => targetX = x; targetY = y - 1
            case East  => targetX = x + 1; targetY = y
            case West  => targetX = x - 1; targetY = y
          }
          initMoving()
          nextState('moving)
        } else {
          //println("too close")
        }
      case Key.LEFT  => targetO = turnLeft()
      case Key.RIGHT => targetO = turnRight()
    }
  }

  state('moving) {
    every(tmpWait) {
      if (stepCnt == steps) {
        snap()
        nextState('loop)
      } else {
        stepCnt += 1
        x += dX
        y += dY
        orientation += dO
        //println("micro step " + stepCnt + " to (" + x + ", " + y + ", " + orientation + ")")
        snapTo(x, y, orientation)
      }
    }
  }

  def initMoving() = {
    dX = (targetX - x) / (1.0*steps)
    dY = (targetY - y) / (1.0*steps)
    // dO = (targetO - orientation) / (1.0*steps)
    dO = 0.0
    stepCnt = 0
  }

}
