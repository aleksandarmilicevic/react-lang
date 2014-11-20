package react.examples.arduino

import react.runtime.ArduinoExecutor
import react.examples._

object Run {

  def apply(port: String) {
    Console.println("arduino test port: " + port)
    //val r = new FollowTheEdge(port)
    val r = new SwipeScan(port)
    //val exec = new ArduinoExecutor(r)
    val exec = new ArduinoExecutor(r, false, Some(100))
    new Remote(r)
    exec.start
  }

}
