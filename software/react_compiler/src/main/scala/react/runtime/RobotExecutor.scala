package react.runtime

import react._
import react.message._
import org.ros.namespace.GraphName
import org.ros.node.{Node, NodeMain, ConnectedNode}
import org.ros.concurrent.CancellableLoop

class RobotExecutor(robot: Robot) extends NodeMain {

  override def getDefaultNodeName: GraphName = {
    GraphName.of("react_runtime/" + robot.id)
  }

  override def onStart(node: ConnectedNode) {
    node.executeCancellableLoop(new CancellableLoop {
      val scheduler = new Scheduler

      override def setup() {
        super.setup()
        //for subscribing and publishing
        robot.setNode(node)
        //register the control loop
        for ( (period, fct) <- robot.tasks )
          scheduler.addTask(period, fct)
      }

      def loop() {
        //TODO
        // non-blocking version
        // get all the expired task at once
        var msg: Seq[Message] = Seq()
        scheduler.waitUntilNextTask match {
          case Some(task) =>
            robot.lock.lock
            try {
              robot.shadow
              task.fct()
              msg = robot.generateMvmt(task.period)
            } finally {
              robot.lock.unlock
            }
            scheduler.reschedule(task)
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
