package react.verification.ghost

import react._
import react.robot._

class UserInput(robot: Robot) extends Ghost {

  def alternatives = 5

  def act(alt: Int) = alt match {
    case 0 => robot.send(Key.UP)
    case 1 => robot.send(Key.DOWN)
    case 2 => robot.send(Key.LEFT)
    case 3 => robot.send(Key.RIGHT)
    case 4 => robot.send(Key.NONE)
    case _ => sys.error("alt should be in [0,4]")
  }

}
