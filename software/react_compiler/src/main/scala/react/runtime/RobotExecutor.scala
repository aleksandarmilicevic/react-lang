package react.runtime

import react._
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
        //register all the listener
        robot.registerListener(node)
        //register the control loop
        for ( (period, fct) <- robot.tasks )
          scheduler.addTask(period, fct)
      }

      def loop() {
        //TODO a non-blocking version
        scheduler.waitUntilNextTask match {
          case Some(task) =>
            robot.lock.lock
            try {
              task.fct()
              scheduler.reschedule(task)
            } finally {
              robot.lock.unlock
            }
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
