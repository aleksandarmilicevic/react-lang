package react.examples

import react._
import react.runtime._
import react.examples.turtle._

class RunTurtleTeleop extends RobotExecutor {

  val robot = new TurtleTeleop(Main.topic)

  new Remote(robot)

}

object Main {

  var topic = ""

  def main(args: Array[String]) {
    if (args.length != 1) {
      sys.error("need exactlty one argument: the namespace of the turtle")
    }
    topic = args(0)
    org.ros.RosRun.main(Array(classOf[RunTurtleTeleop].getName()))
  }

}

