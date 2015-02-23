package react.examples.arduino

import react.runtime.ArduinoExecutor
import react.examples._
import react.Robot

object Run {

  def apply(port: String) {
    apply(new SwipeScan(port), port)
  }

  def apply(r: Robot, port: String, pollPeriod: Option[Int] = Some(333)) {
    Console.println("arduino test port: " + port)
    val exec = new ArduinoExecutor(r, false, pollPeriod)
    new Remote(r)
    exec.start
  }

}
