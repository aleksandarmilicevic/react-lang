package react.verification

import react.Executor

/** a class that depends on an executor */
trait Executed {

  @transient protected var exec: Executor = null

  def register(exec: Executor) = {
    this.exec = exec
  }

  def deregister(exec: Executor) = {
    this.exec = null
  }

}
