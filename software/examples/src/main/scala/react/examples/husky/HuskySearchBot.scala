package react.examples.husky

import react._
import react.robot._
import react.message._
import react.utils.Env
import react.examples._
import react.examples.husky._

class HuskySearchBot(_id: String) extends HuskyRobot(_id) with FsmController {

  val botWaitTime = Env.getInt("REACT_BOT_WAIT_TIME", 300)

  initialState('init)

  state('init) {
    every(100) { if (poseUpdated) nextState('main) }
  }

  state('main) {
    every(botWaitTime) {
      val whatNext = new java.util.Random().nextInt(7)
      whatNext match {
        case TURN_LEFT  => turnLeft();  //println("<--")
        case TURN_RIGHT => turnRight(); //println("-->")
        case _ => 
          if (distanceUpdated && frontDistance > safeDistance) {
            goStraightBy(1)
            //println("^^^")
          }
      }
    }
  }

}
