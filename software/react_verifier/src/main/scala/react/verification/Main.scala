package react.verification

import org.ros.RosCore
import org.ros.RosRun
import org.ros.node.{DefaultNodeMainExecutor, NodeConfiguration, NodeMain}
import react.utils._

object Main {

  var core: RosCore = null

  def startCore {
    Logger("verification.Main", LogNotice, "starting core")
    core = RosCore.newPrivate()
    core.start()
    try {
      core.awaitStart()
    } catch {
      case e: Exception => throw new RuntimeException(e)
    }
    Logger("verification.Main", LogNotice, "core started")
  }

  def shutdownCore {
    if (core != null) {
      Logger("verification.Main", LogNotice, "shuting down core")
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
