package react.examples

import react._
import react.runtime._
import react.examples.turtle._
import react.examples.husky._

class RunTurtleTeleop extends RosExecutor {
  //define the robot we are working on
  val robot = new TurtleTeleop(Main.topic)
  //start the gui to control the robot
  new Remote(robot)
}

class RunTurtleRandom extends RosExecutor {
  //define the robot we are working on
  val robot = new TurtleRandom(Main.topic)
}

class RunHuskyTeleop extends RosExecutor {
  //define the robot we are working on
  val robot = new HuskyTeleop(Main.topic)
  //start the gui to control the robot
  new Remote(robot)
}

class RunHuskyGrid extends RosExecutor {
  //define the robot we are working on
  val robot = new HuskyGrid(Main.topic)
  //start the gui to control the robot
  new Remote(robot)
}

class RunHuskyGrid2 extends RosExecutor {
  //define the robot we are working on
  val robot = new HuskyGrid2(Main.topic)
  //start the gui to control the robot
  new Remote(robot)
}

object Main {

  var topic = ""

  def main(args: Array[String]) {
    if (args.length != 2) {
      sys.error("need exactlty two argument: the type of controller and the namespace of the turtle")
    }
    topic = args(1)
    args(0) match {
      case "teleop" =>
        org.ros.RosRun.main(Array(classOf[RunTurtleTeleop].getName))
      case "random" =>
        org.ros.RosRun.main(Array(classOf[RunTurtleRandom].getName))
      case "husky" =>
        org.ros.RosRun.main(Array(classOf[RunHuskyTeleop].getName))
      case "huskyG" =>
        org.ros.RosRun.main(Array(classOf[RunHuskyGrid].getName))
      case "huskyA" =>
        org.ros.RosRun.main(Array(classOf[RunHuskyGrid2].getName))
      case other =>
        println("unknown controller '" + other + "', using teleop instead")
        org.ros.RosRun.main(Array(classOf[RunTurtleTeleop].getName))
    }
  }

}

