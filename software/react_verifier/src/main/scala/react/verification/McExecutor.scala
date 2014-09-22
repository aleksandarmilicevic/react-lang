package react.verification

import react._
import react.message._
import react.runtime._
import org.ros.namespace.GraphName
import org.ros.node.{Node, NodeMain, ConnectedNode}
import org.ros.node.topic._
import org.ros.message.MessageListener
import org.ros.concurrent.CancellableLoop
import react.utils._
import java.util.concurrent.{Semaphore,TimeUnit}
import java.util.concurrent.atomic.AtomicInteger

class MySubscriber[T](sub: Subscriber[T]) extends Subscriber[T] {
  import org.ros.node.topic._
  val cnt = new AtomicInteger()
  def addMessageListener(listener: MessageListener[T], limit: Int) {
    sub.addMessageListener(listener, limit)
    cnt.incrementAndGet
  }
  def addMessageListener(listener: MessageListener[T]) {
    sub.addMessageListener(listener)
    cnt.incrementAndGet
  }
  def getLatchMode = sub.getLatchMode
  def addSubscriberListener(listener: SubscriberListener[T]) {
    sub.addSubscriberListener(listener)
  }
  def shutdown = sub.shutdown
  def shutdown(timeout: Long, unit: TimeUnit) = sub.shutdown(timeout, unit) 
  def getTopicMessageType = sub.getTopicMessageType
  def getTopicName = sub.getTopicName
}

abstract class McExecutor extends NodeMain with Executor with McOptions {

  val world: World
  
  protected var node: ConnectedNode = null 

  val scheduler = new Scheduler

  //TODO wrap the pub/sub to avoid the type casting


  class NamespaceWrapper(namespace: String) extends Executor {

    private def mkTopic(t: String) = {
      react.utils.RosUtils.mayAddPrefix(namespace, t)
    }

    def publish[T](topic: String, typeName: String, message: T) = {
      McExecutor.this.publish[T](mkTopic(topic), typeName, message)
    }

    def delayedPublish[T](delay: Int, topic: String, typeName: String, message: T) = {
      McExecutor.this.delayedPublish(delay, mkTopic(topic), typeName, message)
    }
    
    def getSubscriber[T](topic: String, typeName: String): org.ros.node.topic.Subscriber[T] = {
      McExecutor.this.getSubscriber[T](mkTopic(topic), typeName)
    }
                
    def convertMessage[N](msg: Message): N = McExecutor.this.convertMessage[N](msg)

    def schedule(t: react.runtime.ScheduledTask) = McExecutor.this.schedule(t)

    def removeCanceledTask: Unit = McExecutor.this.removeCanceledTask
  
    override def messageDelivered: Unit = McExecutor.this.messageDelivered

  }

  private val registrationSleep = 100

  private val cnt = new AtomicInteger()
  private val pending = new Semaphore(1000)
  pending.drainPermits()

  def waitUntilDelivered {
    val toDeliver = cnt.get
    cnt.set(0)
    val done = pending.tryAcquire(toDeliver, 500, TimeUnit.MILLISECONDS)
    val a = pending.availablePermits
    if (!done) {
      Logger("McExecutor", LogWarning, "could not make sure that all messages were delivered ("+a+" < " + toDeliver + ")")
    } else if (a > 0) {
      Logger("McExecutor", LogWarning, "delivered more than expected: "+a)
    }
    pending.drainPermits()
  }
  override def messageDelivered {
    //println("messageDelivered")
    Logger("McExecutor", LogDebug, "message delivered")
    pending.release()
  }
  
  private val publishers = scala.collection.mutable.Map[String, Any]()
  def getPublisher[T](topic: String, typeName: String): org.ros.node.topic.Publisher[T] = {
    if (publishers contains topic) {
      publishers(topic).asInstanceOf[org.ros.node.topic.Publisher[T]]
    } else {
      val p = node.newPublisher[T](topic, typeName)
      Thread.sleep(registrationSleep)
      publishers += (topic -> p)
      p
    }
  }
  
  def publish[T](topic: String, typeName: String, message: T) = {
    val pub = getPublisher[T](topic, typeName)
    val ns = getSubscribed[T](topic, typeName) //messages to deliver
    Logger("McExecutor", LogDebug, "publishing on " + topic + "[" + typeName + "](" + ns + ")")
    cnt.addAndGet(ns)
    pub.publish(message)
  }

  def delayedPublish[T](delay: Int, topic: String, typeName: String, message: T) = {
    val pub = getPublisher[T](topic, typeName)
    scheduler.addSingleTask("delayed publish on " + topic, delay, () => pub.publish(message))
  }

  private val subscribers = scala.collection.mutable.Map[String, Any]()
  def getSubscriber[T](topic: String, typeName: String): Subscriber[T] = {
    if (subscribers contains topic) {
      subscribers(topic).asInstanceOf[MySubscriber[T]]
    } else {
      val p = new MySubscriber[T](node.newSubscriber[T](topic, typeName))
      Thread.sleep(registrationSleep)
      subscribers += (topic -> p)
      p
    }
  }
  def getSubscribed[T](topic: String, typeName: String): Int = {
    if (subscribers contains topic) {
      subscribers(topic).asInstanceOf[MySubscriber[T]].cnt.get
    } else {
      0
    }
  }

  def convertMessage[N](msg: Message): N = {
    Message.toMessage(node, msg).asInstanceOf[N] //TODO something nicer to avoid the casting
  }
  
  def schedule(t: react.runtime.ScheduledTask) = scheduler.schedule(t)

  def removeCanceledTask: Unit = scheduler.removeCanceled

  def getMcOptions: McOptions = this

  ///////////////////
  // The ROS stuff //
  ///////////////////

  override def getDefaultNodeName: GraphName = {
    GraphName.of("react/verifier")
  }

  var mc: ModelChecker = null 

  override def onStart(n: ConnectedNode) {
    node = n
    node.executeCancellableLoop(new CancellableLoop {

    mc = new ModelChecker(world, McExecutor.this, scheduler, getMcOptions)

      override def setup() {
        super.setup()
        //set the exec to everybody
        for (r <- world.robots) r.setExec(new NamespaceWrapper(r.id))
        for (m <- world.models) m.register(McExecutor.this)
        for (g <- world.ghosts) g.register(McExecutor.this)
        //initialize the MC
        mc.init
      }

      def loop() {
        val somethingNew = mc.oneStep
        if (!somethingNew) {
          if (mc != null)
            mc.printStats
          Main.shutdownCore
          System.exit(0)
        }
      }
    })
  }

  override def onShutdown(node: Node) {
    if (mc != null)
      mc.printStats
  }

  override def onShutdownComplete(node: Node) {
  }

  override def onError(node: Node, throwable: Throwable) {
    Console.err.println("exception : " + throwable)
    throwable.printStackTrace(Console.err)
  }

}
