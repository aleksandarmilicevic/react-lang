package react.examples

import react._
import react.runtime._
import react.utils._
import react.examples.turtle._
import react.examples.husky._
import dzufferey.arg._
import dzufferey.utils.Logger

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

class RunHuskyTeleopMvmt extends RosExecutor {
  //define the robot we are working on
  val robot = new HuskyTeleopMvmt(Main.topic)
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

  val robot = new HuskySearchBot(Main.topic)
}

class RunHuskyPathfinder extends RosExecutor {
  val robot = new HuskyPathfinder(Main.topic)
}

object Main extends Options with react.verification.McOptions {

  newOption("-v", Unit(() => Logger.moreVerbose), "increase the verbosity level.")
  newOption("-q", Unit(() => Logger.lessVerbose), "decrease the verbosity level.")

  newOption("-bfs",         Unit(() => bfs = true),         "BFS state-space exploration")
  newOption("-dfs",         Unit(() => bfs = false),        "DFS space-space exploration")
  newOption("-tb",          Int(l => timeBound = l),        "bound the time horizon (in milliseconds)")
  newOption("-transient",   Unit(() => keepTransient = true), "keep the transient states (faster for small systems, requires more memory)")
  newOption("-pc",          Int( i => periodCoeff = i),     "coefficient to multiply the period used when building the time quotient")
  newOption("-tf",          String( s => traceFile = s),    "save the trace as an SVG image")
  newOption("-cf",          String( s => coverageFile = s), "save the coverage as an SVG image")
  newOption("-t",           Int( i => nbrWorlds = i),       "how many world to run in parallel (default: 4, if possible)")
  newOption("-noROS",       Unit( () => bypassROS = true),  "bypass ROS, less faithfull to actual program but faster")
  newOption("-concretize",  Unit( () => withConcretize = true), "more correct way of handling the discretization")

  val usage = "..."

  var topic = ""

  def main(args: Array[java.lang.String]) {
    apply(args) // preprocess the args
    val (cls, topic) =
      if (input.length == 0)
        sys.error("need at least on argument: the class to launch")
      else if (input.length == 1)
        (input(0), "")
      else {
        (input(1), input(0))
      }
    println("topic = " + topic)
    this.topic = topic
    cls match {
      case "teleop" =>
        org.ros.RosRun.main(Array(classOf[RunTurtleTeleop].getName))
      case "random" =>
        org.ros.RosRun.main(Array(classOf[RunTurtleRandom].getName))
      case "husky" =>
        org.ros.RosRun.main(Array(classOf[RunHuskyTeleop].getName))
      case "mvmt" =>
        org.ros.RosRun.main(Array(classOf[RunHuskyTeleopMvmt].getName))
      case "huskyG" =>
        org.ros.RosRun.main(Array(classOf[RunHuskyGrid].getName))
      case "huskyA" =>
        org.ros.RosRun.main(Array(classOf[RunHuskyGrid2].getName))
      case "huskyS" =>
        org.ros.RosRun.main(Array(classOf[RunHuskyGridSnap].getName))
      case "huskyB" =>
        org.ros.RosRun.main(Array(classOf[RunHuskySearchBot].getName))
      case "huskyP" =>
        org.ros.RosRun.main(Array(classOf[RunHuskyPathfinder].getName))
      case "huskyV" =>
        val runner = new react.verification.McRunner(this, (() => new HuskyVerif))
        runner.run
      case "huskyV2" =>
        val runner = new react.verification.McRunner(this, (() => new HuskyVerif2))
        runner.run
      case "tests" =>
        tests.RunVerif(this, topic)
      case "arduino" =>
        //arduino.Run(new arduino.SwipeScan(topic), topic)
        arduino.Run(new arduino.FollowTheEdge(topic, true), topic)
        //arduino.Run(new arduino.TriTeleop(topic), topic, None)
      case "arduinoV" =>
        bypassROS = true
        arduino.RunV(topic, this)
      case other =>
        throw new RuntimeException("Unknown robot: " + other)
    }
  }

}

