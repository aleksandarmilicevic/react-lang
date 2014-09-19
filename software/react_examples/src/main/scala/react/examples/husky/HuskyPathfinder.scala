package react.examples.husky

import scala.util.control.Breaks._

import react._
import react.robot._
import react.message._
import react.examples._
import react.examples.husky._

class HuskyPathfinder(_id: String) extends HuskyRobot(_id) with FsmController {

  val pathfinderWaitTime = getEnvI("REACT_PATHFINDER_WAIT_TIME", 300)
  val spinWaitTime = getEnvI("REACT_PATHFINDER_SPIN_WAIT_TIME", 5)

  // val steps = List((1,-1), (1,0), (1,1), (0,-1), (0,1), (-1,-1), (-1,0), (-1,1))
  val steps = List((1,0), (0,-1), (0,1), (-1,0))

  var destX = -2
  var destY = -8

  var history = Set[(Long,Long)]()

  def goalDist(x: Long, y: Long): Double = {
    val dx = Math.abs(x - destX)
    val dy = Math.abs(y - destY)
    Math.sqrt(dx*dx + dy*dy)
  }

  initialState('init)

  state('init) {
    every(100) {
      if (poseUpdated) {
        //TODO: call /gazebo/model_states to find the position of the goal object
        nextState('main)
      }
    }
  }

  var moves: List[(Long,Long)] = List()
  var actions: List[Int] = List()

  state('main) {
    every(pathfinderWaitTime) {
      val (ix,iy,io) = currentIntegralPosition
      history += ((ix,iy))

      if (goalDist(ix, iy) == 0) {
        nextState('found)
      } else {
        if (actions.isEmpty) {
          if (moves.isEmpty) {
            moves = steps.map{
              case (dx,dy) => (ix+dx, iy+dy)
            }.filter{ (tt) =>
              !history.contains(tt)
            }.sortWith { (e1, e2) =>
              goalDist(e1._1, e1._2) < goalDist(e2._1, e2._2)
            }
          }

          if (!moves.isEmpty)  {
            actions = toOps((ix,iy), moves.head)
            moves = moves.tail
          }
        }

        if (!actions.isEmpty) {
          val op = actions.head
          actions = actions.tail
          var actionFailed = false
          op match {
            case TURN_LEFT   => turnLeft();  //println("<--")
            case TURN_RIGHT  => turnRight(); //println("-->")
            case GO_STRAIGHT =>
              if (distanceUpdated && frontDistance > safeDistance) {
                goStraightBy(1); //println("^^^")
              } else {
                actionFailed = true
              }
          }
          if (actionFailed) {
            actions = List() // clear actions to advance to next move
          } else {
            if (actions.isEmpty)
              moves = List() // move successfull -> clear other moves
          }
        }
      }
    }
  }

  state('found) {
    every(spinWaitTime) {
      rotateTo(this.orientation + math.Pi/90)
    }
  }

}

