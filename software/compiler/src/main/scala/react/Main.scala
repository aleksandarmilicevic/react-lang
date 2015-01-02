package react

import react.runtime._

object Main {

  def main(args: Array[String]) {
    //launch the runtime
    org.ros.RosRun.main(Array(classOf[Runtime].getName()) ++ args)
  }
}
