package react.verification

import react._
import react.message._
import react.runtime._
import org.ros.namespace.GraphName
import org.ros.node.{Node, NodeMain, ConnectedNode}
import org.ros.concurrent.CancellableLoop

abstract class RosExecutor extends NodeMain with Executor {

  //TODO all the robots ?
  val robot: Robot

  protected var node: ConnectedNode = null 

  val scheduler = new Scheduler

  //TODO wrap the pub/sub to avoid the type casting
  
  private val publishers = scala.collection.mutable.Map[String, Any]()
  def getPublisher[T](topic: String, typeName: String): org.ros.node.topic.Publisher[T] = {
    val t = react.utils.RosUtils.mayAddPrefix(robot.id, topic)
    if (publishers contains t) {
      publishers(t).asInstanceOf[org.ros.node.topic.Publisher[T]]
    } else {
      val p = node.newPublisher[T](t, typeName)
      publishers += (t -> p)
      p
    }
  }
  
  def publish[T](topic: String, typeName: String, message: T) = {
    val pub = getPublisher[T](topic, typeName)
    pub.publish(message)
  }

  def delayedPublish[T](delay: Int, topic: String, typeName: String, message: T) = {
    val pub = getPublisher[T](topic, typeName)
    scheduler.addSingleTask(delay, () => pub.publish(message))
  }

  private val subscribers = scala.collection.mutable.Map[String, Any]()
  def getSubscriber[T](topic: String, typeName: String): org.ros.node.topic.Subscriber[T] = {
    val t = react.utils.RosUtils.mayAddPrefix(robot.id, topic)
    if (subscribers contains t) {
      subscribers(t).asInstanceOf[org.ros.node.topic.Subscriber[T]]
    } else {
      val p = node.newSubscriber[T](t, typeName)
      subscribers += (t -> p)
      p
    }
  }

  def convertMessage[N](msg: Message): N = {
    Message.toMessage(node, msg).asInstanceOf[N] //TODO something nicer to avoid the casting
  }
  
  def schedule(t: react.runtime.ScheduledTask) = scheduler.schedule(t)

  def removeCanceledTask: Unit = scheduler.removeCanceled

  ///////////////////
  // The ROS stuff //
  ///////////////////

  override def getDefaultNodeName: GraphName = {
    GraphName.of("react/" + robot.id)
  }

  override def onStart(n: ConnectedNode) {
    node = n
    node.executeCancellableLoop(new CancellableLoop {

      override def setup() {
        super.setup()
        //TODO ...
      }

      def loop() {
        //TODO ...
      }
    })
  }

  override def onShutdown(node: Node) {
    //TODO ...
  }

  override def onShutdownComplete(node: Node) {
  }

  override def onError(node: Node, throwable: Throwable) {
    //TODO display more information, like state of the system when the error was throw, what input, ...
    Console.err.println(robot.id + " has thrown: " + throwable)
    throwable.printStackTrace(Console.err)
  }

}
