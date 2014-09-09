package react.examples

import react._
import react.runtime._
import react.utils._
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

class RunHuskyGridSnap extends RosExecutor {
  //define the robot we are working on
  val robot = new HuskyGridSnap(Main.topic)
  //start the gui to control the robot
  new Remote(robot)
}

class RunHuskySearchBot extends RosExecutor {
  //define the robot we are working on
  val robot = new HuskySearchBot(Main.topic)
  //start the gui to control the robot
  new Remote(robot)
}

object Main extends Options {

  newOption("-v", Arg.Unit(() => Logger.moreVerbose), "increase the verbosity level.")
  newOption("-q", Arg.Unit(() => Logger.lessVerbose), "decrease the verbosity level.")

  val usage = "..."

  var topic = ""

  def main(args: Array[String]) {
    apply(args) // preprocess the args
    if (input.length != 2) {
      sys.error("need exactlty two argument: the type of controller and the namespace of the turtle")
    }
    topic = input(0)
    input(1) match {
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
      case "huskyV" =>
        org.ros.RosRun.main(Array(classOf[RunHuskyVerif].getName))
      case "huskyS" =>
        org.ros.RosRun.main(Array(classOf[RunHuskyGridSnap].getName))
      case "huskyB" =>
        org.ros.RosRun.main(Array(classOf[RunHuskySearchBot].getName))
      case other =>
        println("unknown controller '" + other + "', using teleop instead")
        org.ros.RosRun.main(Array(classOf[RunTurtleTeleop].getName))
    }
  }

}

