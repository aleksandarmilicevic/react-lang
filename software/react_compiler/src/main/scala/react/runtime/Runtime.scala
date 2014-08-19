package react.runtime

import org.ros.namespace.GraphName
import org.ros.message.MessageListener
import org.ros.node.{Node, NodeMain, ConnectedNode}
import org.ros.concurrent.CancellableLoop
import react.message._

class Runtime extends NodeMain {
  
  val poses = scala.collection.mutable.HashMap[String, PoseStamped]()

  val nodeId = "/react"

  val publishPeriod = 500

  override def getDefaultNodeName: GraphName = {
    GraphName.of(nodeId)
  }

  override def onStart(node: ConnectedNode) {

    //subcribe to the 'react/poses' topic
    val sub = node.newSubscriber[geometry_msgs.PoseStamped](nodeId + "/pose", geometry_msgs.PoseStamped._TYPE)
    val listener = new MessageListener[geometry_msgs.PoseStamped]{
      def onNewMessage(message: geometry_msgs.PoseStamped) {
        val id = message.getHeader.getFrameId
        poses += (id -> Message.from(message))
      }
    }
    sub.addMessageListener(listener)

    node.executeCancellableLoop(new CancellableLoop {

      private def now = java.lang.System.currentTimeMillis()
      var last = now
    
      val pub = node.newPublisher[nav_msgs.Path](nodeId + "/poses", nav_msgs.Path._TYPE)

      var cnt = -1

      def loop() {
        val t = now
        if (t - last > publishPeriod) {
          last = t
          cnt += 1
          val path = Path(Header(cnt, Message.time(System.currentTimeMillis()), ""), poses.values.toArray)
          val asRos = Message.to(node, path)
          pub.publish(asRos)
        }
      }

    })
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
