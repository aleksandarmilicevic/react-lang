package react.examples.husky

import scala.util.control.Breaks._

import react._
import react.robot._
import react.message._
import react.examples._
import react.examples.husky._

class HuskyPathfinder(_id: String) extends HuskyRobot(_id) with FsmController {

  val pathfinderWaitTime = getEnvI("REACT_PATHFINDER_WAIT_TIME", 500)
  // val steps = List((1,-1), (1,0), (1,1), (0,-1), (0,1), (-1,-1), (-1,0), (-1,1))
  val steps = List((1,0), (0,-1), (0,1), (-1,0))

  var destX = -1
  var destY = -6.5

  // var history = scala.collection.mmutable.Set[(Long,Long)]()
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

  state('main) {
    every(pathfinderWaitTime) {
      val (ix,iy,io) = currentIntegralPosition
      history += ((ix,iy))
      val options = steps.map{
        case (dx,dy) => (ix+dx, iy+dy)
      }.filter{ (tt) =>
        !history.contains(tt)
      }.sortWith { (e1, e2) =>
        goalDist(e1._1, e1._2) < goalDist(e2._1, e2._2)
      }

      breakable {
        options.foreach { (t) =>
          toOp((ix,iy), t) match {
            case TURN_LEFT   => turnLeft();  println("<--"); break
            case TURN_RIGHT  => turnRight(); println("-->"); break
            case GO_STRAIGHT =>
              if (distanceUpdated && frontDistance > safeDistance) {
                goStraightBy(1)
                println("^^^")
                break
              }
          }
        }
      }

    }
  }

}



  // state('main) {
  //   val (x,y,o) = currentIntegralPosition
  //   // val t: (Long,Long) = (x,y)
  //   // history += t
  //   // val options = steps.map{ 
  //   //   case (dx,dy) => (x+dx, y+dy)
  //   // }.filter{ (tt) =>
  //   //   !history.contains(tt)
  //   // }
  // }
