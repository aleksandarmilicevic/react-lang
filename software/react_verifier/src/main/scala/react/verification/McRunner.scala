package react.verification

import react.utils._
import react.verification.modelchecker._

/** Model checker options */
trait McOptions {
  /* how many ghosts steps per period */
  //var ghostSteps = 1
  var timeBound = -1
  var bfs = true
  var keepTransient = false
  var periodCoeff = 1
  var traceFile = ""
  var coverageFile = ""
  var nbrWorlds = 4
  var bypassROS = false
}

class McRunner(opts: McOptions, newWorld: () => World) {

  protected def mkProxy = new WorldProxy( newWorld(), opts )

  var mc: ModelChecker = null
  var prs: Array[WorldProxy] = null

  protected def shutdown: Nothing = {
    prs.foreach(_.shutdown)
    val m = mc
    if (m != null) m.printStats
    Runtime.getRuntime().halt(0)
    sys.error("halt did not work")
  }

  def run: Nothing = {
    val pr = mkProxy
    prs =
      if (pr.worldAgnostic) {
        pr +: (for (i <- (1 until opts.nbrWorlds).par) yield mkProxy).toArray
      } else {
        Array(pr)
      }
    mc = new ModelChecker(prs, opts)
    mc.init
    while(mc.oneStep) {}
    shutdown
  }

  Runtime.getRuntime().addShutdownHook(
    new Thread() {
      override def run() {
        Console.err.println("shutdown hook")
        shutdown
      }
    }
  )


}
