package react.runtime

import org.ros.namespace.GraphName
import org.ros.node.{Node, NodeMain}

class Runtime extends NodeMain {

  override def getDefaultNodeName: GraphName = {
    new GraphName("react_runtime")
  }

  override def onStart(node: ConnectedNode) {
  }

  override def onShutdown(node: Node) {
  }

  override def onShutdownComplete(node: Node) {
  }

  override def onError(node: Node, throwable: Throwable) {
  }

}

//  http://docs.rosjava.googlecode.com/hg/rosjava_core/html/javadoc/org/ros/concurrent/CancellableLoop.html
//  http://docs.rosjava.googlecode.com/hg/rosjava_core/html/javadoc/org/ros/message/MessageListener.html
//  http://docs.rosjava.googlecode.com/hg/rosjava_core/html/javadoc/org/ros/node/topic/Subscriber.html
//  http://docs.rosjava.googlecode.com/hg/rosjava_core/html/javadoc/org/ros/node/topic/Publisher.html
//  http://docs.rosjava.googlecode.com/hg/rosjava_core/html/javadoc/org/ros/node/ConnectedNode.html
