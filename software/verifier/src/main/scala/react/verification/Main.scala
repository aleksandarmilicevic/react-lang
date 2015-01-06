package react.verification

import org.ros.RosCore
import org.ros.RosRun
import org.ros.node.{DefaultNodeMainExecutor, NodeConfiguration, NodeMain}
import dzufferey.utils.LogLevel._
import dzufferey.utils.Logger

object Main {

  var core: RosCore = null

  def startCore {
    Logger("verification.Main", Notice, "starting core")
    core = RosCore.newPrivate()
    core.start()
    try {
      core.awaitStart()
    } catch {
      case e: Exception => throw new RuntimeException(e)
    }
    Logger("verification.Main", Notice, "core started")
  }

  def shutdownCore {
    if (core != null) {
      Logger("verification.Main", Notice, "shuting down core")
      core.shutdown
      core = null
    }
  }

  /** the first argument should be the name of the class to run */
  def main(args: Array[String]) {
    RosRun.main(args)
    //org.ros.RosRun.main(Array(classOf[Runtime].getName()) ++ args)
  }

  def runVerifier(node: NodeMain) = {
    startCore
    val config = NodeConfiguration.newPrivate()
    val exec = DefaultNodeMainExecutor.newDefault()
    config.setMasterUri(core.getUri())
    config.setNodeName("ReactVerifier")
    exec.execute(node, config)
  }

}
