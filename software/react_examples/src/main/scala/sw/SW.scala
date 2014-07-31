package react.examples.sg

import react._

/* a segway-like robot */

class SW extends Robot {

  var x = 0
  var y = 0
  var orientation = 0.0

  var vx = 0
  var vy = 0

  every(1000){ case () =>
    x += vx
    y += vy
    orientation = math.atan2(vx, vy)
  }

  on[Key.Value]{
    case Key.UP =>    vy += 1
    case Key.DOWN =>  vy -= 1
    case Key.LEFT =>  vx += 1
    case Key.RIGHT => vx -= 1
    case Key.NONE =>
  }

}
