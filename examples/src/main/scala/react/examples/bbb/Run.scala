package react.examples.bbb

import react.runtime.BulldogExecutor
import react.runtime.bulldog._
import react.examples._
import react.Robot

object Run {

  def apply(r: Robot, hal: HAL) {
    val exec = new BulldogExecutor(r, hal)
    exec.start
  }

}
