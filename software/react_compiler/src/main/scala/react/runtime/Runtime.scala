package react.runtime

import org.ros.namespace.GraphName
import org.ros.node.{Node, NodeMain, ConnectedNode}

class Runtime extends NodeMain {

  override def getDefaultNodeName: GraphName = {
    GraphName.of("react_runtime")
  }

  override def onStart(node: ConnectedNode) {
    //TODO subscribe wait for new node to connect to the react_runtime node
  }

  override def onShutdown(node: Node) {
  }

  override def onShutdownComplete(node: Node) {
  }

  override def onError(node: Node, throwable: Throwable) {
  }

}

// http://rosjava.github.io/rosjava_core/hydro/javadoc/overview-summary.html
//  http://docs.rosjava.googlecode.com/hg/rosjava_core/html/javadoc/org/ros/concurrent/CancellableLoop.html
//  http://docs.rosjava.googlecode.com/hg/rosjava_core/html/javadoc/org/ros/message/MessageListener.html
//  http://docs.rosjava.googlecode.com/hg/rosjava_core/html/javadoc/org/ros/node/topic/Subscriber.html
//  http://docs.rosjava.googlecode.com/hg/rosjava_core/html/javadoc/org/ros/node/topic/Publisher.html
//  http://docs.rosjava.googlecode.com/hg/rosjava_core/html/javadoc/org/ros/node/ConnectedNode.html
