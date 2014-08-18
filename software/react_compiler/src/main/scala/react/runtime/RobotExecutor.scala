package react.runtime

import react._
import react.message._
import org.ros.namespace.GraphName
import org.ros.node.{Node, NodeMain, ConnectedNode}
import org.ros.concurrent.CancellableLoop

abstract class RobotExecutor extends NodeMain {

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

  def subscribe[T](topic: String, typeName: String, handler: T => Unit) = {
    val listener = new org.ros.message.MessageListener[T]{
      def onNewMessage(message: T) {
          robot.lock.lock()
          try {
            handler(message)
          } finally {
            robot.lock.unlock
          }
        }
    }
    val sub = getSubscriber[T](topic, typeName)
    sub.addMessageListener(listener)
  }

  def convertMessage[N](msg: Message): N = {
    Message.toMessage(node, msg).asInstanceOf[N] //TODO something nicer to avoid the casting
  }

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
        //for subscribing and publishing
        robot.setExec(RobotExecutor.this)
        //register the control loop
        for ( (period, fct) <- robot.tasks )
          scheduler.addPeriodicTask(period, fct)
      }

      def loop() {
        //TODO
        // non-blocking version
        // get all the expired task at once
        //var msg: Seq[Message] = Seq()
        scheduler.waitUntilNextTask match {
          case Some(task) =>
            robot.lock.lock
            try {
              //robot.shadow
              task.fct()
              //msg = robot.generateMvmt(task.period)
            } finally {
              robot.lock.unlock
            }
            if (task.period > 0) {
              scheduler.reschedule(task)
            }
            //TODO send the messages
          case None => ()
        }
      }
    })
  }

  override def onShutdown(node: Node) {
    //todo graceful shutdown: deregister the handlers
  }

  override def onShutdownComplete(node: Node) {
  }

  override def onError(node: Node, throwable: Throwable) {
    Console.err.println(robot.id + " has thrown: " + throwable)
    throwable.printStackTrace(Console.err)
  }

}
