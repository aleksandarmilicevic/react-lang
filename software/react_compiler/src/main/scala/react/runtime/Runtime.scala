package react.runtime

import org.ros.namespace.GraphName
import org.ros.message.MessageListener
import org.ros.node.{Node, NodeMain, ConnectedNode}

class Runtime extends NodeMain {

  val nodeId = "react"

  override def getDefaultNodeName: GraphName = {
    GraphName.of(nodeId)
  }

  override def onStart(node: ConnectedNode) {
    val subs = node.newSubscriber[std_msgs.String](nodeId + "/register", std_msgs.String._TYPE)
    val listener = new MessageListener[std_msgs.String]{
      def onNewMessage(message: std_msgs.String) {
        //TODO
        //  wait for new node to connect to the react runtime node
        //  then create the corresponding robot within react
        sys.error("TODO")
      }
    }
  }

  override def onShutdown(node: Node) {
  }

  override def onShutdownComplete(node: Node) {
  }

  override def onError(node: Node, throwable: Throwable) {
    Console.err.println("REACT runtime, exception thrown: " + throwable)
    throwable.printStackTrace(Console.err)
  }

}

// http://rosjava.github.io/rosjava_core/hydro/javadoc/overview-summary.html
//  http://docs.rosjava.googlecode.com/hg/rosjava_core/html/javadoc/org/ros/concurrent/CancellableLoop.html
//  http://docs.rosjava.googlecode.com/hg/rosjava_core/html/javadoc/org/ros/message/MessageListener.html
//  http://docs.rosjava.googlecode.com/hg/rosjava_core/html/javadoc/org/ros/node/topic/Subscriber.html
//  http://docs.rosjava.googlecode.com/hg/rosjava_core/html/javadoc/org/ros/node/topic/Publisher.html
//  http://docs.rosjava.googlecode.com/hg/rosjava_core/html/javadoc/org/ros/node/ConnectedNode.html
