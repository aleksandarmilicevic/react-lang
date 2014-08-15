package react.examples.sg

import react._
import react.message._
import react.examples._

/* a segway-like robot (i.e. enough degree of freedom to move anywhere) */

class SW(_id: String) extends GroundRobot(_id) {

  var vx = 0
  var vy = 0

  every(200){
    x += vx
    y += vy
    orientation = math.atan2(vy, vx)
  }

  on {
    case Key.UP =>    vy += 1
    case Key.DOWN =>  vy -= 1
    case Key.LEFT =>  vx += 1
    case Key.RIGHT => vx -= 1
    case Key.NONE =>
  }

  sensor[Range]("front"){
    case Range(_, _, _, _, _, range) =>
      if (range < 10) {
        vx = 0
        vy = 0
      }
  }

}
