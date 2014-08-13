package react.examples.turtle

import react._
import react.message._
import react.examples._


class TurtleTeleop(_id: String) extends Robot(_id) {

  val none = 0
  val left = 1
  val right = 2
  val up = 3
  val down = 4

  var nextAction = none

  //angular/linear scale
  val scaleA = math.Pi / 2 // 90 degree
  val scaleL = 1.0

  //messages for the different actions
  val twistLeft  = Twist(Vector3(0,0,0), Vector3(0,0, scaleA))
  val twistRight = Twist(Vector3(0,0,0), Vector3(0,0,-scaleA))
  val twistUp    = Twist(Vector3( scaleL,0,0), Vector3(0,0,0))
  val twistDown  = Twist(Vector3(-scaleL,0,0), Vector3(0,0,0))
  
  //input from keyboard
  on {
    case Key.UP =>    nextAction = up
    case Key.DOWN =>  nextAction = down
    case Key.LEFT =>  nextAction = left
    case Key.RIGHT => nextAction = right
    case Key.NONE =>  nextAction = none
  }

  //dispatch a message every second (that's how turtlesim works)
  every(1000){
    nextAction match {
      case `none` =>  ()
      case `left` =>  publish("cmd_vel", twistLeft)
      case `right` => publish("cmd_vel", twistRight)
      case `up` =>    publish("cmd_vel", twistUp)
      case `down` =>  publish("cmd_vel", twistDown)
      case _ =>       sys.error("unknown action")
    }
    nextAction = none
  }

}
