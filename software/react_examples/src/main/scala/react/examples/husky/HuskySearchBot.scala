package react.examples.husky

import react._
import react.robot._
import react.message._
import react.examples._
import react.examples.husky._

class HuskySearchBot(_id: String) extends HuskyRobot(_id) with FsmController {

  val botWaitTime = getEnvI("REACT_BOT_WAIT_TIME", 500)

  val maxX = 10
  val minX = -10
  val maxY = 10
  val minY = -10;

  var myX = 0.0
  var myY = 0.0
  var myO = 0.0

  initialState('init)

  state('init) {
    every(100) { if (poseUpdated) nextState('main) }
  }

  state('main) {
    every(botWaitTime) {
      val whatNext = new java.util.Random().nextInt(7)
      whatNext match {
        case TURN_LEFT  => turnLeft();  println("<--")
        case TURN_RIGHT => turnRight(); println("-->")
        case _ => 
          if (distanceUpdated && frontDistance > safeDistance) {
            goStraightBy(1)
            println("^^^")
          }
      }
    }
  }

}
