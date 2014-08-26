package react.verification

object Main {

  /** the first argument should be the name of the class to run */
  def main(args: Array[String]) {
    org.ros.RosRun.main(args)
    //org.ros.RosRun.main(Array(classOf[Runtime].getName()) ++ args)
  }

}
