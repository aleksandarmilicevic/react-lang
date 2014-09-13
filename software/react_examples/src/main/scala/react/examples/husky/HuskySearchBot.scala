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

  // var targetX = 0.0
  // var targetY = 0.0

  var myX = 0.0
  var myY = 0.0
  var myO = 0.0

  initialState('init)

  state('init) {
    every(100) { if (poseUpdated) nextState('main) }
  }

  state('main) {
    every(botWaitTime) {
      if (frontDistance > safeDistance) {
        val whatNext = new java.util.Random().nextInt(7)
        whatNext match {
          case 0 => turnLeft();      println("<--")
          case 1 => turnRight();     println("-->")
          case _ => goStraightBy(1); println("^^^")
        }        
      } else {
        turnLeft()
        println("---->")
      }
    }
  }

}
