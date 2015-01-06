package react.utils

import dzufferey.utils.SysCmd

object RosUtils {

  def mayAddPrefix(prefix: String, topicName: String) = {
    //println("prefix " + prefix)
    //println("topic " + topicName)
    if (topicName startsWith "/") topicName
    else prefix + "/" + topicName
  }

  def getServiceList = {
    val (res, out, err) = SysCmd.apply(Array("rosservice", "list"), None)
    if (res == 0) out
    else sys.error(err)
  }

  def getServiceType(name: String) = {
    val (res, out, err) = SysCmd.apply(Array("rosservice", "type", name), None)
    if (res == 0) out
    else sys.error(err)
  }

  def expandType(typeName: String) = {
    val (res, out, err) = SysCmd.apply(Array("rossrv", "show", typeName), None)
    if (res == 0) out
    else sys.error(err)
  }

}
