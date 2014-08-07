package react.examples.sg

import react._

/* a segway-like robot (i.e. enough degree of freedom to move anywhere) */

class SW extends GroundRobot {

  var vx = 0
  var vy = 0

  every(1000){ () =>
    x += vx
    y += vy
    theta = math.atan2(vx, vy)
  }

  on[Key.Value]{
    case Key.UP =>    vy += 1
    case Key.DOWN =>  vy -= 1
    case Key.LEFT =>  vx += 1
    case Key.RIGHT => vx -= 1
    case Key.NONE =>
  }

}
